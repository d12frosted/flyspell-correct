;;; flyspell-correct.el --- correcting words with flyspell via custom interface -*- lexical-binding: t -*-
;;
;; Copyright (c) 2016 Boris Buliga
;;
;; Author: Boris Buliga <d12frosted@gmail.com>
;; URL: https://github.com/d12frosted/flyspell-correct
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;; In order to use this package set the value of `flyspell-correct-interface' to
;; any of available interfaces (predefined are `flyspell-correct-ivy',
;; `flyspell-correct-helm' and `flyspell-correct-popup'). For example,
;;
;; (setq flyspell-correct-interface 'flyspell-correct-ivy)
;;
;; After that, just call `flyspell-correct-word-generic' with cursor on
;; misspelled word. You can also bind it by adding this to your init.el file:
;;
;; (define-key flyspell-mode-map (kbd "C-;") 'flyspell-correct-word-generic)
;;
;; When invoked, it will show the list of corrections suggested by Flyspell.
;; `ivy', `helm' and `popup' also allows to save unknown word to your
;; dictionary, accept this spelling in current buffer or whole session.
;;
;;; Code:
;;

;; Requires

(require 'flyspell)

;; Variables

(defvar flyspell-correct-interface nil
  "Interface for `flyspell-correct-word-generic'.
Available predefined interfaces are `flyspell-correct-ivy',
`flyspell-correct-helm' and `flyspell-correct-popup'. It has to
be function that accepts two arguments - candidates and
misspelled word. It has to return either replacement word
or (command, word) tuple that will be passed to
`flyspell-do-correct'.")

;; Ivy interface

(declare-function ivy-read "ext:ivy.el"
                  (prompt collection &optional predicate
                          require-match initial-input history
                          preselect keymap update-fn sort action
                          unwind re-builder matcher
                          dynamic-collection caller))

(defun flyspell-correct-ivy (candidates word)
  "Run `ivy-read' for the given CANDIDATES given by flyspell for the WORD.
Return a selected word to use as a replacement or a tuple
of (command, word) to be used by `flyspell-do-correct'."
  (let* (result
         (action-default (lambda (x) (setq result x)))
         (action-save-word (lambda (_) (setq result (cons 'save word))))
         (action-accept-session (lambda (_) (setq result (cons 'session word))))
         (action-accept-buffer (lambda (_) (setq result (cons 'buffer word))))
         (action `(1
                   ("o" ,action-default "correct")
                   ("s" ,action-save-word "Save")
                   ("S" ,action-accept-session "Accept (session)")
                   ("b" ,action-accept-buffer "Accept (buffer)"))))
    (ivy-read (format "Suggestions for \"%s\" in dictionary \"%s\": "
                      word (or ispell-local-dictionary
                               ispell-dictionary
                               "Default"))
              candidates
              :action action
              :caller 'flyspell-correct-ivy)
    result))

;; Helm interface

(declare-function helm "ext:helm.el"
                  (&optional sources input prompt resume
                             preselect buffer keymap default
                             history allow-nest
                             other-local-vars))
(declare-function helm-build-sync-source "ext:helm-source.el"
                  (name &rest args))

(defun flyspell-correct--helm-always-match (_)
  "Return non-nil for any CANDIDATE."
  t)

(defun flyspell-correct--helm-option-candidates (word)
  "Return a set of options for the given WORD."
  (let ((opts (list (cons (format "Save \"%s\"" word)
                          (cons 'save word))
                    (cons (format "Accept (session) \"%s\"" word)
                          (cons 'session word))
                    (cons (format "Accept (buffer) \"%s\"" word)
                          (cons 'buffer word)))))
    (unless (string= helm-pattern "")
      (setq opts
            (append opts
                    (list (cons (format "Save \"%s\"" helm-pattern)
                                (cons 'save helm-pattern))
                          (cons (format "Accept (session) \"%s\"" helm-pattern)
                                (cons 'session helm-pattern))
                          (cons (format "Accept (buffer) \"%s\"" helm-pattern)
                                (cons 'buffer helm-pattern))))))
    opts))

(defun flyspell-correct-helm (candidates word)
  "Run helm for the given CANDIDATES given by flyspell for the WORD.
Return a selected word to use as a replacement or a tuple
of (command, word) to be used by `flyspell-do-correct'."
  (helm :sources (list (helm-build-sync-source
                           (format "Suggestions for \"%s\" in dictionary \"%s\""
                                   word (or ispell-local-dictionary
                                            ispell-dictionary
                                            "Default"))
                         :candidates candidates
                         :action 'identity
                         :candidate-number-limit 9999
                         :fuzzy-match t)
                       (helm-build-sync-source "Options"
                         :candidates (lambda ()
                                       (flyspell-correct--helm-option-candidates word))
                         :action 'identity
                         :candidate-number-limit 9999
                         :match 'flyspell-correct--helm-always-match
                         :volatile t))
        :buffer "*Helm Flyspell*"
        :prompt "Correction: "))

;; Popup interface

(declare-function popup-menu* "ext:popup.el"
                  (list &optional point (around t)
                        (width (popup-preferred-width list))
                        (height 15) max-width margin margin-left
                        margin-right scroll-bar symbol parent
                        parent-offset cursor (keymap popup-menu-keymap)
                        (fallback 'popup-menu-fallback)
                        help-delay nowait prompt
                        isearch (isearch-filter
                        'popup-isearch-filter-list)
                        (isearch-cursor-color popup-isearch-cursor-color)
                        (isearch-keymap popup-isearch-keymap)
                        isearch-callback initial-index))

(declare-function popup-make-item "ext:popup.el"
                  (name &optional value face mouse-face
                        selection-face sublist document symbol
                        summary))

(defun flyspell-correct-popup (candidates word)
  "Run popup for the given CANDIDATES given by flyspell for the WORD.
Return a selected word to use as a replacement or a tuple
of (command, word) to be used by `flyspell-do-correct'."
  (popup-menu*
   (append
    candidates
    (list
     (popup-make-item (format "Save \"%s\"" word)
                      :value (cons 'save word))
     (popup-make-item (format "Accept (session) \"%s\"" word)
                      :value (cons 'session word))
     (popup-make-item (format "Accept (buffer) \"%s\"" word)
                      :value (cons 'buffer word))))
   :margin t))

;; Implementation

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

(provide 'flyspell-correct)

;; Local Variables:
;; no-byte-compile: t
;; END:
;;; flyspell-correct.el ends here
