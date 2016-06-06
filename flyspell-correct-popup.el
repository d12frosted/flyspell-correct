;;; flyspell-correct-popup.el --- correcting words with flyspell via popup interface
;;
;; Copyright (c) 2016 Boris Buliga
;;
;; Author: Boris Buliga <d12frosted@gmail.com>
;; URL: https://github.com/d12frosted/flyspell-correct
;; Package-Version: 0.1.0
;; Package-Requires: ((flyspell-correct "0.1.0") (popup "0.5.3"))
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

;; Interface implementation

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

(setq flyspell-correct-interface #'flyspell-correct-popup)

(provide 'flyspell-correct-popup)

;;; flyspell-correct-popup.el ends here
