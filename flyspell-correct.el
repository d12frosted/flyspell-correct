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
;; For usage instructions refer README file.
;;
;;; Code:
;;

;; Requires

(require 'flyspell)

;; Variables

(defvar flyspell-correct-interface nil
  "Interface for `flyspell-correct-word'. Available predefined
  interfaces are `flyspell-correct-ivy', `flyspell-correct-helm'
  and `flyspell-correct-popup'.")

;; Ivy interface

(declare-function ivy-read "ext:ivy.el" (PROMPT COLLECTION &optional PREDICATE REQUIRE-MATCH INITIAL-INPUT HISTORY
                                                PRESELECT KEYMAP UPDATE-FN SORT ACTION UNWIND RE-BUILDER MATCHER
                                                DYNAMIC-COLLECTION CALLER))

(defun flyspell-correct-ivy (candidates word)
  "Run `ivy-read' for the given CANDIDATES given by flyspell for the WORD.
Return a selected word to use as a replacement."
  (let* (result
         (action-default (lambda (x) (setq result x)))
         (action-save-word (lambda (x) (setq result (cons 'save word))))
         (action-accept-session (lambda (x) (setq result (cons 'session word))))
         (action-accept-buffer (lambda (x) (setq result (cons 'buffer word))))
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

(declare-function helm "ext:helm.el" (&optional SOURCES INPUT PROMPT RESUME PRESELECT BUFFER KEYMAP DEFAULT HISTORY
                                                ALLOW-NEST OTHER-LOCAL-VARS))
(declare-function helm-build-sync-source "ext:helm-source.el" (NAME &rest ARGS))

(defun flyspell-correct--helm-always-match (candidate)
  "Return true for any CANDIDATE."
  t)

(defun flyspell-correct--helm-option-candidates (word)
  "Return a set of options for the given WORD."
  (let ((opts (list (cons (format "Save \"%s\"" word) (cons 'save word))
                    (cons (format "Accept (session) \"%s\"" word) (cons 'session word))
                    (cons (format "Accept (buffer) \"%s\"" word) (cons 'buffer word)))))
    (unless (string= helm-pattern "")
      (setq opts (append opts (list (cons (format "Save \"%s\"" helm-pattern) (cons 'save helm-pattern))
                                    (cons (format "Accept (session) \"%s\"" helm-pattern) (cons 'session helm-pattern))
                                    (cons (format "Accept (buffer) \"%s\"" helm-pattern) (cons 'buffer helm-pattern))))))
    opts))

(defun flyspell-correct-helm (candidates word)
  "Run helm for the given CANDIDATES given by flyspell for the WORD.
Return a selected word to use as a replacement or a tuple
of (command, word) to be used by flyspell-do-correct."
  (helm :sources (list (helm-build-sync-source (format "Suggestions for \"%s\" in dictionary \"%s\""
                                                       word (or ispell-local-dictionary
                                                                ispell-dictionary
                                                                "Default"))
                         :candidates candidates
                         :action 'identity
                         :candidate-number-limit 9999
                         :fuzzy-match t
                         )
                       (helm-build-sync-source "Options"
                         :candidates '(lambda ()
                                        (let ((tmp word))
                                           (flyspell-correct--helm-option-candidates tmp)))
                         :action 'identity
                         :candidate-number-limit 9999
                         :match 'flyspell-correct--helm-always-match
                         :volatile t
                         )
                       )
        :buffer "*Helm Flyspell*"
        :prompt "Correction: "))

;; Popup interface

(declare-function popup-menu* "ext:popup.el"
                  (LIST &optional POINT (AROUND t) (WIDTH (popup-preferred-width list))
                        (HEIGHT 15) MAX-WIDTH MARGIN MARGIN-LEFT MARGIN-RIGHT SCROLL-BAR SYMBOL PARENT
                        PARENT-OFFSET CURSOR (KEYMAP popup-menu-keymap) (FALLBACK 'popup-menu-fallback)
                        HELP-DELAY NOWAIT PROMPT ISEARCH (ISEARCH-FILTER 'popup-isearch-filter-list)
                        (ISEARCH-CURSOR-COLOR popup-isearch-cursor-color)
                        (ISEARCH-KEYMAP popup-isearch-keymap) ISEARCH-CALLBACK INITIAL-INDEX))

(declare-function popup-make-item "ext:popup.el"
                  (NAME &optional VALUE FACE MOUSE-FACE SELECTION-FACE SUBLIST DOCUMENT
                        SYMBOL SUMMARY))

(defun flyspell-correct-popup (candidates word)
  "Run popup for the given CANDIDATES given by flyspell for the WORD.
Return a selected word to use as a replacement or a tuple
of (command, word) to be used by flyspell-do-correct."
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
        (let ((start (car (cdr word)))
              (end (car (cdr (cdr word))))
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
            (let ((res (funcall flyspell-correct-interface (third poss) word)))
              (cond ((stringp res)
                     (flyspell-do-correct res poss word cursor-location start end opoint))
                    (t
                     (let ((cmd (car res))
                           (wrd (cdr res)))
                       (if (string= wrd word)
                           (flyspell-do-correct cmd poss wrd cursor-location start end opoint)
                         (progn
                           (flyspell-do-correct cmd poss wrd cursor-location start end opoint)
                           (flyspell-do-correct wrd poss word cursor-location start end opoint)))))))))
          (ispell-pdict-save t)))))

(provide 'flyspell-correct)
;;; flyspell-correct.el ends here
