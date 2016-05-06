;;; flyspell-correct.el --- correcting words with flyspell via custom interface
;;
;; Copyright (c) 2016 Boris Buliga
;;
;; Author: Boris Buliga <d12frosted@gmail.com>
;; URL: https://github.com/d12frosted/flyspell-correct
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:
;; For usage instructions refer README file.

;;; Code:

;; For lexical-let
(eval-when-compile
  (require 'cl))

;; Requires

(require 'flyspell)
;; (require 'ivy)
;; (require 'helm)

;; Variables

(defvar flyspell-correct-interface nil
  "Interface for `flyspell-correct-word'. Available predefined
  interfaces are `flyspell-correct/ivy' and
  `flyspell-correct/helm'.")

;; Ivy interface

(defun flyspell-correct/ivy (candidates word)
  "Run helm for the given CANDIDATES given by flyspell for the WORD.
Return a selected word to use as a replacement."
  (ivy-read (format "Suggestions for \"%s\" in dictionary \"%s\": "
                    word (or ispell-local-dictionary
                             ispell-dictionary
                             "Default")) candidates))

;; Helm interface

(defun flyspell-correct//helm-always-match (candidate)
  "Return true for any CANDIDATE."
  t)

(defun flyspell-correct//helm-option-candidates (word)
  "Return a set of options for the given WORD."
  (let ((opts (list (cons (format "Save \"%s\"" word) (cons 'save word))
                    (cons (format "Accept (session) \"%s\"" word) (cons 'session word))
                    (cons (format "Accept (buffer) \"%s\"" word) (cons 'buffer word)))))
    (unless (string= helm-pattern "")
      (setq opts (append opts (list (cons (format "Save \"%s\"" helm-pattern) (cons 'save helm-pattern))
                                    (cons (format "Accept (session) \"%s\"" helm-pattern) (cons 'session helm-pattern))
                                    (cons (format "Accept (buffer) \"%s\"" helm-pattern) (cons 'buffer helm-pattern))))))
    opts))

(defun flyspell-correct/helm (candidates word)
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
                                        (lexical-let ((tmp word))
                                           (flyspell-correct//helm-option-candidates tmp)))
                         :action 'identity
                         :candidate-number-limit 9999
                         :match 'flyspell-correct//helm-always-match
                         :volatile t
                         )
                       )
        :buffer "*Helm Flyspell*"
        :prompt "Correction: "))

;; Implementation

;;;###autoload
(defun flyspell-correct/word ()
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
