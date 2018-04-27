;;; flyspell-correct-popup.el --- correcting words with flyspell via popup interface
;;
;; Copyright (c) 2016-2018 Boris Buliga
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; URL: https://github.com/d12frosted/flyspell-correct
;; Package-Version: 0.4.0
;; Package-Requires: ((flyspell-correct "0.4.0") (popup "0.5.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;; This package provides popup interface for flyspell-correct package.
;;
;; Points of interest are `flyspell-correct-word-generic' and
;; `flyspell-correct-previous-word-generic'.
;;
;;; Code:
;;

;; Requires

(require 'flyspell-correct)
(require 'popup)

;; Interface implementation

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
                      :value (cons 'buffer word))))
   :margin t))

(setq flyspell-correct-interface #'flyspell-correct-popup)

(provide 'flyspell-correct-popup)

;;; flyspell-correct-popup.el ends here
