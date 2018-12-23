;;; flyspell-correct.el --- correcting words with flyspell via custom interface
;;
;; Copyright (c) 2016-2018 Boris Buliga
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; URL: https://github.com/d12frosted/flyspell-correct
;; Package-version: 0.5.0
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;;
;; This package provides functionality for correcting words via custom
;; interfaces. There are several functions for this:
;;
;; - `flyspell-correct-at-point' - to correct word at point.
;; - `flyspell-correct-previous' to correct any visible word before the point.
;; - `flyspell-correct-next' to correct any visible word after the point.
;; - `flyspell-correct-wrapper' - a beefed wrapper for
;;   `flyspell-correct-previous' and `flyspell-correct-next' allowing one to
;;   correct many words at once (rapid flow) and change correction direction.
;;
;; In most cases the last function is the most convenient, so don't forget to
;; bind it.
;;
;;   (define-key flyspell-mode-map (kbd "C-;") 'flyspell-correct-wrapper)
;;
;; When invoked, it will show the list of corrections suggested by Flyspell.
;;
;; Most interfaces also allow you to save the new word to your dictionary,
;; accept this spelling in current buffer or for a whole session, or even skip
;; this word (useful in a rapid flow).
;;
;; Default interface is implemented using `completing-read', but it's highly
;; advised to use `flyspell-correct-ido' (which comes bundled with this package)
;; or any interface provided by following packages: `flyspell-correct-ivy',
;; `flyspell-correct-helm' and `flyspell-correct-popup'.
;;
;; In order to use `flyspell-correct-ido' interface instead of default
;; `flyspell-correct-dummy', place following snippet in your 'init.el' file.
;;
;;   (require 'flyspell-correct-ido)
;;
;; It's easy to implement your own interface for `flyspell-correct'. Checkout
;; documentation for `flyspell-correct-interface' variable.
;;
;; For more information about this and related packages, please refer to
;; attached README.org file.
;;
;;; Code:
;;

;; Requires

(require 'flyspell)

;; Variables

(defvar flyspell-correct-interface #'flyspell-correct-dummy
  "Interface for `flyspell-correct-at-point'.
It has to be function that accepts two arguments - candidates and
misspelled word. It has to return either replacement word
or (command, word) tuple that will be passed to
`flyspell-do-correct'.")

;;; Default interface
;;

(defun flyspell-correct-dummy (candidates word)
  "Run `completing-read' for the given CANDIDATES.

List of CANDIDATES is given by flyspell for the WORD.

Return a selected word to use as a replacement or a tuple
of (command, word) to be used by `flyspell-do-correct'."
  (completing-read (format "Correcting '%s': " word) candidates))

;;; On point word correction
;;

(defalias 'flyspell-correct-word-generic 'flyspell-correct-at-point)

;;;###autoload
(defun flyspell-correct-at-point ()
  "Correct word before point using `flyspell-correct-interface'.
Adapted from `flyspell-correct-word-before-point'."
  (interactive)
  (unless flyspell-correct-interface
    (error "Could not correct word because `flyspell-correct-interface' is not set"))
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
          (ispell-send-string "%\n")    ;put in verbose mode
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
            (message "%s is correct" (funcall ispell-format-word-function word))
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
                       (unless (eq cmd 'skip)
                         (flyspell-do-correct
                          cmd poss wrd cursor-location start end opoint)))))
              (ispell-pdict-save t))))))))

;;; Previous word correction
;;

(defalias 'flyspell-correct-previous-word-generic 'flyspell-correct-previous)

;;;###autoload
(defun flyspell-correct-previous (position)
  "Correct the first misspelled word that occurs before POSITION.
But don't look beyond what's visible on the screen.

Uses `flyspell-correct-at-point' function for correction.

With a prefix argument, automatically continues to all prior misspelled words in the buffer."
  (interactive "d")
  (flyspell-correct-move position nil current-prefix-arg))

;;; Next word correction
;;

(defalias 'flyspell-correct-next-word-generic 'flyspell-correct-next)

;;;###autoload
(defun flyspell-correct-next (position)
  "Correct the first misspelled word that occurs after POSITION.

Uses `flyspell-correct-at-point' function for correction.

With a prefix argument, automatically continues to all further
misspelled words in the buffer."
  (interactive "d")
  (flyspell-correct-move position t current-prefix-arg))

;;; Generic helpers
;;

;;;###autoload
(defun flyspell-correct-wrapper (arg)
  "Correct spelling error in a dwim fashion based on ARG.

- One \\[universal-argument] enables rapid mode.
- Two \\[universal-argument]'s changes direction of spelling
  errors search.
- Three \\[universal-argument]'s changes direction of spelling
  errors search and enables rapid mode."
  (interactive "P")
  (if (or (not (mark)) (/= (mark) (point)))
	    (push-mark (point) t))

  (let ((flyspell-forward-direction nil)
		    (flyspell-rapid nil))
    (cond
     ((equal current-prefix-arg '(4))  ; C-u = rapid
	    (setq flyspell-rapid t))
     ((equal current-prefix-arg '(16)) ; C-u C-u = change direction
      (setq flyspell-forward-direction t))
     ((equal current-prefix-arg '(64)) ; C-u C-u C-u = do both
	    (setq flyspell-rapid t)
	    (setq flyspell-forward-direction t)))

    (flyspell-correct-move (point) flyspell-forward-direction flyspell-rapid)))

;;;###autoload
(defun flyspell-correct-move (position &optional forward rapid)
  "Correct the first misspelled word that occurs before POSITION.

Uses `flyspell-correct-at-point' function for correction.

With FORWARD set non-nil, check forward instead of backward.

With RAPID set non-nil, automatically continues in direction
until all errors in buffer have been addressed."
  ;; NOTE: The way I may be pushing the mark may possibly be more
  ;; idiomatically done using the opoint arg of
  ;; `flyspell-correct-word-before-point'.
  (interactive "d")
  (save-excursion
    (let ((top (window-start))
          (bot (window-end))
          (incorrect-word-pos)
          (position-at-incorrect-word))

      ;; narrow the region
      (overlay-recenter (point))

      (let ((overlay-list
             (if forward
                 (overlays-in (- position 1) (point-max))
               (overlays-in (point-min) (+ position 1))))
            (overlay 'dummy-value))
        (while overlay
          (setq overlay (car-safe overlay-list))
          (setq overlay-list (cdr-safe overlay-list))
          (when (and overlay
                     (flyspell-overlay-p overlay))
            (setq position-at-incorrect-word
                  (and (<= (overlay-start overlay) position)
                       (>= (overlay-end overlay) position)))
            (setq incorrect-word-pos (overlay-start overlay))
            (let ((scroll (> incorrect-word-pos (window-end))))
              (goto-char incorrect-word-pos)
              (when scroll (recenter)))

            ;; try to correct word `flyspell-correct-at-point' returns t when
            ;; there is nothing to correct. In such case we just skip current
            ;; word.
            (unless (flyspell-correct-at-point)
              (when (/= (mark) (point)) (push-mark (point) t))
              (when (not rapid) (setq overlay nil))))))

      (when incorrect-word-pos
        (goto-char incorrect-word-pos)
        (forward-word)
        (when (= (mark) (point)) (pop-mark))))))

;;; Automatically correct
;; based on `flyspell-popup-auto-correct-mode'

(defcustom flyspell-correct-auto-delay 1.6
  "Delay in seconds before `flyspell-correct-previous' is called.
Use floating point numbers to express fractions of seconds."
  :group 'flyspell
  :type 'number
  :safe #'numberp)

(defvar flyspell-correct-auto-mode-interface nil
  "Interface to use in `flyspell-correct-auto-mode'.
When set to nil `flyspell-correct-interface' is used.")

(defvar flyspell-correct--auto-timer nil
  "Timer to automatically call `flyspell-correct-previous'.")
(make-variable-buffer-local 'flyspell-correct--auto-timer)

(defvar flyspell-correct--auto-active-p nil)
(make-variable-buffer-local 'flyspell-correct--auto-active-p)

(defun flyspell-correct-auto-cancel-timer ()
  "Cancel auto correct timer."
  (when flyspell-correct--auto-timer
    (cancel-timer flyspell-correct--auto-timer)
    (setq flyspell-correct--auto-timer nil)))

(defun flyspell-correct-auto-soon ()
  "Call `flyspell-correct-previous' delayed."
  (flyspell-correct-auto-cancel-timer)
  (when (and flyspell-mode
             (not (bound-and-true-p flyspell-correct--auto-active-p)))
    (setq
     flyspell-correct--auto-timer
     (run-at-time
      flyspell-correct-auto-delay
      nil
      (lambda ()
        (flyspell-correct-auto-cancel-timer)
        (when (and flyspell-mode
                   (not (bound-and-true-p flyspell-correct--auto-active-p)))
          (setq flyspell-correct--auto-active-p t)
          (with-local-quit
            (let ((flyspell-correct-interface
                   (if (bound-and-true-p flyspell-correct-auto-mode-interface)
                       flyspell-correct-auto-mode-interface
                     flyspell-correct-interface)))
              (call-interactively #'flyspell-correct-previous)))
          (setq flyspell-correct--auto-active-p nil)))))))

;;;###autoload
(define-minor-mode flyspell-correct-auto-mode
  "Minor mode for automatically correcting word at point.

Take my advice and don't use this functionality unless you find
`flyspell-correct-previous' function useless for your purposes.
Seriously, just try named function for completion. You can find
more info in comment[1].

[1]:
https://github.com/syl20bnr/spacemacs/issues/6209#issuecomment-274320376"
  :group 'flyspell
  :lighter "auto-correct"
  (if flyspell-correct-auto-mode
      (progn
        (add-hook 'post-command-hook 'flyspell-correct-auto-soon nil 'local))
    (remove-hook 'post-command-hook 'flyspell-correct-auto-soon 'local)))

(provide 'flyspell-correct)

;;; flyspell-correct.el ends here
