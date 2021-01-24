;;; flyspell-correct-popup.el --- Correcting words with flyspell via popup interface -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2016-2021 Boris Buliga
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; URL: https://github.com/d12frosted/flyspell-correct
;; Version: 0.6.1
;; Package-Requires: ((flyspell-correct "0.6.1") (popup "0.5.3") (emacs "24"))
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;; This package provides popup interface for flyspell-correct package.
;;
;; Points of interest are `flyspell-correct-wrapper',
;; `flyspell-correct-previous' and `flyspell-correct-next'.
;;
;; Example usage:
;;
;;   (require 'flyspell-correct-popup)
;;   (define-key flyspell-mode-map (kbd "C-;") 'flyspell-correct-wrapper)
;;
;; Or via use-package:
;;
;;   (use-package flyspell-correct-popup
;;     :bind ("C-M-;" . flyspell-correct-wrapper)
;;     :init
;;     (setq flyspell-correct-interface #'flyspell-correct-popup))
;;
;;; Code:
;;

;; Requires

(require 'flyspell-correct)
(require 'popup)

;; Interface implementation

;;;###autoload
(defun flyspell-correct-popup (candidates word)
  "Run `popup-menu*' for the given CANDIDATES.

List of CANDIDATES is given by flyspell for the WORD.

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
                      :value (cons 'buffer word))
     (popup-make-item (format "Skip \"%s\"" word)
                      :value (cons 'skip word))
     (popup-make-item "Stop"
                      :value (cons 'stop word))))
   :margin t))

(setq flyspell-correct-interface #'flyspell-correct-popup)

(provide 'flyspell-correct-popup)

;;; flyspell-correct-popup.el ends here
