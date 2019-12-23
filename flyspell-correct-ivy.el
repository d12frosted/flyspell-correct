;;; flyspell-correct-ivy.el --- Correcting words with flyspell via ivy interface -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2016-2019 Boris Buliga
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; URL: https://github.com/d12frosted/flyspell-correct
;; Version: 0.5.0
;; Package-Requires: ((flyspell-correct "0.5.0") (ivy "0.8.0"))
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;; This package provides ivy interface for flyspell-correct package.
;;
;; Points of interest are `flyspell-correct-wrapper',
;; `flyspell-correct-previous' and `flyspell-correct-next'.
;;
;; Example usage:
;;
;;   (require 'flyspell-correct-ivy)
;;   (define-key flyspell-mode-map (kbd "C-;") 'flyspell-correct-wrapper)
;;
;; Or via use-package:
;;
;;   (use-package flyspell-correct-ivy
;;     :bind ("C-M-;" . flyspell-correct-wrapper)
;;     :init
;;     (setq flyspell-correct-interface #'flyspell-correct-ivy))
;;
;;; Code:
;;

;; Requires

(require 'flyspell-correct)
(require 'ivy)

;; Interface implementation

;;;###autoload
(defun flyspell-correct-ivy (candidates word)
  "Run `ivy-read' for the given CANDIDATES.

List of CANDIDATES is given by flyspell for the WORD.

Return a selected word to use as a replacement or a tuple
of (command, word) to be used by `flyspell-do-correct'."
  (let* (result
         (action-default (lambda (x) (setq result x)))
         (action-save-word (lambda (_) (setq result (cons 'save word))))
         (action-accept-session (lambda (_) (setq result (cons 'session word))))
         (action-accept-buffer (lambda (_) (setq result (cons 'buffer word))))
         (action-skip-word (lambda (_) (setq result (cons 'skip word))))
         (action `(1
                   ("o" ,action-default "correct")
                   ("s" ,action-save-word "Save")
                   ("S" ,action-accept-session "Accept (session)")
                   ("b" ,action-accept-buffer "Accept (buffer)")
                   ("k" ,action-skip-word "Skip"))))
    (ivy-read (format "Suggestions for \"%s\" in dictionary \"%s\": "
                      word (or ispell-local-dictionary
                               ispell-dictionary
                               "Default"))
              candidates
              :action action
              :caller 'flyspell-correct-ivy)
    result))

(setq flyspell-correct-interface #'flyspell-correct-ivy)

(provide 'flyspell-correct-ivy)

;;; flyspell-correct-ivy.el ends here
