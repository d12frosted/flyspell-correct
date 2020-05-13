;;; flyspell-correct-ivy.el --- Correcting words with flyspell via ivy interface -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2016-2019 Boris Buliga
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; URL: https://github.com/d12frosted/flyspell-correct
;; Version: 0.6.1
;; Package-Requires: ((flyspell-correct "0.6.1") (ivy "0.8.0") (emacs "24.3"))
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

(defvar flyspell-correct-ivy-map (make-sparse-keymap)
  "Keymap used in the `flyspell-correct-ivy' minibuffer.")

(defvar-local flyspell-correct-ivy--result nil
  "Result of `flyspell-correct-ivy'.

See `flyspell-correct-interface' for more information.")

;;;###autoload
(defun flyspell-correct-ivy (candidates word)
  "Run `ivy-read' for the given CANDIDATES.

List of CANDIDATES is given by flyspell for the WORD.

Return result according to `flyspell-correct-interface'
specification."
  (setq flyspell-correct-ivy--result nil)
  (let* ((action-default
          (lambda (x)
            (setq flyspell-correct-ivy--result x)))
         (action-save-word
          (lambda (x)
            (setq flyspell-correct-ivy--result
                  (cons 'save (if (member x candidates) word x)))))
         (action-accept-session
          (lambda (x)
            (setq flyspell-correct-ivy--result
                  (cons 'session (if (member x candidates) word x)))))
         (action-accept-buffer
          (lambda (x)
            (setq flyspell-correct-ivy--result
                  (cons 'buffer (if (member x candidates) word x)))))
         (action-skip-word
          (lambda (x)
            (setq flyspell-correct-ivy--result
                  (cons 'skip (if (member x candidates) word x)))))
         (action-stop
          (lambda (x)
            (setq flyspell-correct-ivy--result
                  (cons 'stop (if (member x candidates) word x)))))
         (action `(1
                   ("o" ,action-default "correct")
                   ("s" ,action-save-word "Save")
                   ("S" ,action-accept-session "Accept (session)")
                   ("b" ,action-accept-buffer "Accept (buffer)")
                   ("k" ,action-skip-word "Skip")
                   ("p" ,action-stop "Stop"))))
    (ivy-read (format "Suggestions for \"%s\" in dictionary \"%s\": "
                      word (or ispell-local-dictionary
                               ispell-dictionary
                               "Default"))
              candidates
              :action action
              :keymap flyspell-correct-ivy-map
              :caller 'flyspell-correct-ivy)
    flyspell-correct-ivy--result))

(setq flyspell-correct-interface #'flyspell-correct-ivy)

(provide 'flyspell-correct-ivy)

;;; flyspell-correct-ivy.el ends here
