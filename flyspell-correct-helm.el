;;; flyspell-correct-helm.el --- correcting words with flyspell via helm interface
;;
;; Copyright (c) 2016 Boris Buliga
;;
;; Author: Boris Buliga <d12frosted@gmail.com>
;; URL: https://github.com/d12frosted/flyspell-correct
;; Package-Version: 0.1.0
;; Package-Requires: ((flyspell-correct "0.1.0") (helm "1.9.0"))
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;; This package provides helm interface for flyspell-correct package.
;;
;; Points of interest are `flyspell-correct-word-generic' and
;; `flyspell-correct-previous-word-generic'.
;;
;;; Code:
;;

;; Requires

(require 'flyspell-correct)
(require 'helm)

;; Interface implementation

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

(setq flyspell-correct-interface #'flyspell-correct-helm)

(provide 'flyspell-correct-helm)

;;; flyspell-correct-helm.el ends here
