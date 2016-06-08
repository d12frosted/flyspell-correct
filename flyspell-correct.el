;;; flyspell-correct.el --- correcting words with flyspell via custom interface
;;
;; Copyright (c) 2016 Boris Buliga
;;
;; Author: Boris Buliga <d12frosted@gmail.com>
;; URL: https://github.com/d12frosted/flyspell-correct
;; Package-version: 0.1
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;;
;; This package provides functionality for correcting words via custom
;; interfaces. There are two functions for this: `flyspell-correct-word-generic'
;; to correct word at point and `flyspell-correct-previous-word-generic' to
;; correct any visible word before point. In most cases second function is more
;; convenient, so don't forget to bind it.
;;
;; (define-key flyspell-mode-map (kbd "C-;") 'flyspell-correct-previous-word-generic)
;;
;; When invoked, it will show the list of corrections suggested by Flyspell.
;; Most interfaces also allow you to save new word to your dictionary, accept
;; this spelling in current buffer or for a whole session.
;;
;; Since this package does not provide any interface for correcting words, it's
;; better to use one of the following packages: `flyspell-correct-ivy',
;; `flyspell-correct-helm' and `flyspell-correct-popup'. The all depend on
;; `flyspell-correct' and just provide interface for it's functionality.
;;
;; But one can easily implement it's own interface for `flyspell-correct'.
;; Checkout documentation for `flyspell-correct-interface' variable.
;;
;; For more information about this and related package, please read attached
;; README.org file.
;;
;;; Code:
;;

;; Requires

(require 'flyspell)

;; Variables

(defvar flyspell-correct-interface nil
  "Interface for `flyspell-correct-word-generic'.
It has to be function that accepts two arguments - candidates and
misspelled word. It has to return either replacement word
or (command, word) tuple that will be passed to
`flyspell-do-correct'.")

;; On point word correction

;;;###autoload
(defun flyspell-correct-word-generic ()
  "Correct word before point using `flyspell-correct-interface'.
Adapted from `flyspell-correct-word-before-point'."
  (interactive)
  ;; use the correct dictionary
  (flyspell-accept-buffer-local-defs)
  (let ((cursor-location (point))
        (word (flyspell-get-word))
        (opoint (point)))
    (if (consp word)
        (let ((start (nth 1 word))
              (end (nth 2 word))
              (word (car word))
              poss ispell-filter)
          ;; now check spelling of word.
          (ispell-send-string "%\n")	;put in verbose mode
          (ispell-send-string (concat "^" word "\n"))
          ;; wait until ispell has processed word
          (while (progn
                   (accept-process-output ispell-process)
                   (not (string= "" (car ispell-filter)))))
          ;; Remove leading empty element
          (setq ispell-filter (cdr ispell-filter))
          ;; ispell process should return something after word is sent.
          ;; Tag word as valid (i.e., skip) otherwise
          (or ispell-filter
              (setq ispell-filter '(*)))
          (if (consp ispell-filter)
              (setq poss (ispell-parse-output (car ispell-filter))))
          (cond
           ((or (eq poss t) (stringp poss))
            ;; don't correct word
            t)
           ((null poss)
            ;; ispell error
            (error "Ispell: error in Ispell process"))
           (t
            ;; The word is incorrect, we have to propose a replacement.
            (let ((res (funcall flyspell-correct-interface (nth 2 poss) word)))
              (cond ((stringp res)
                     (flyspell-do-correct res poss word cursor-location start end opoint))
                    (t
                     (let ((cmd (car res))
                           (wrd (cdr res)))
                       (flyspell-do-correct
                        cmd poss wrd cursor-location start end opoint)))))))
          (ispell-pdict-save t)))))

;;; Previous word correction
;; mostly stolen from flyspell.el

(defvar flyspell-correct-previous-word--pos nil
  "Holds the start of the first incorrect word before point.")

(defun flyspell-correct-previous-word-hook ()
  "Hook to track successive calls to `flyspell-correct-previous-word-generic'.
Sets `flyspell-correct-previous-word--pos' to nil"
  (interactive)
  (remove-hook 'pre-command-hook (function flyspell-correct-previous-word-hook) t)
  (unless (eq this-command (function flyspell-correct-previous-word-generic))
    (setq flyspell-correct-previous-word--pos nil)))

;;;###autoload
(defun flyspell-correct-previous-word-generic (position)
  "Correct the first misspelled word that occurs before point.
But don't look beyond what's visible on the screen.

Uses `flyspell-correct-word-generic' function for correction."
  (interactive "d")

  (let ((top (window-start))
        (bot (window-end)))
    (save-excursion
      (save-restriction
        (narrow-to-region top bot)
        (overlay-recenter (point))

        (add-hook 'pre-command-hook
                  (function flyspell-correct-previous-word-hook) t t)

        (unless flyspell-correct-previous-word--pos
          ;; only reset if a new overlay exists
          (setq flyspell-correct-previous-word--pos nil)

          (let ((overlay-list (overlays-in (point-min) position))
                (point-at-incorrect (not (null (overlays-in position position))))
                (new-overlay 'dummy-value))

            (when point-at-incorrect
              (setq new-overlay (car (last overlay-list))))

            ;; search for previous (new) flyspell overlay
            (while (and new-overlay
                        (not point-at-incorrect)
                        (or (not (flyspell-overlay-p new-overlay))
                            ;; check if its face has changed
                            (not (eq (get-char-property
                                      (overlay-start new-overlay) 'face)
                                     'flyspell-incorrect))))
              (setq new-overlay (car-safe overlay-list))
              (setq overlay-list (cdr-safe overlay-list)))

            ;; if nothing new exits new-overlay should be nil
            (if new-overlay ;; the length of the word may change so go to the start
                (setq flyspell-correct-previous-word--pos
                      (overlay-start new-overlay)))))

        (when flyspell-correct-previous-word--pos
          (save-excursion
            (goto-char flyspell-correct-previous-word--pos)
            (let ((ispell-following-word t)) ;; point is at start
              (if (numberp flyspell-correct-previous-word--pos)
                  (goto-char flyspell-correct-previous-word--pos))
              (flyspell-correct-word-generic))
            ;; the point may have moved so reset this
            (setq flyspell-correct-previous-word--pos (point))))))))

(provide 'flyspell-correct)

;;; flyspell-correct.el ends here
