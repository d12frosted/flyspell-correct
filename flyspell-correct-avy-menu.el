;;; flyspell-correct-avy-menu.el --- Correcting words with flyspell via avy-menu interface -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2016-2021 Boris Buliga;
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;;         Clemens Radermacher <clemera@posteo.net>
;; URL: https://github.com/d12frosted/flyspell-correct
;; Version: 0.6.1
;; Package-Requires: ((flyspell-correct "0.6.1") (avy-menu "0.1.1") (emacs "24"))
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;; This package provides avy-menu interface for flyspell-correct package.
;;
;; Points of interest are `flyspell-correct-wrapper',
;; `flyspell-correct-previous' and `flyspell-correct-next'.
;;
;; Example usage:
;;
;;   (require 'flyspell-correct-avy-menu)
;;   (define-key flyspell-mode-map (kbd "C-;") 'flyspell-correct-wrapper)
;;
;; Or via use-package:
;;
;;   (use-package flyspell-correct-avy-menu
;;     :bind ("C-M-;" . flyspell-correct-wrapper)
;;     :init
;;     (setq flyspell-correct-interface #'flyspell-correct-avy-menu))
;;
;;; Code:
;;

;; Requires

(require 'flyspell-correct)
(require 'avy-menu)

;; Interface implementation

;;;###autoload
(defun flyspell-correct-avy-menu (candidates word)
  "Run `avy-menu' for the given CANDIDATES.

List of CANDIDATES is given by flyspell for the WORD.

Return a selected word to use as a replacement or a tuple
of (command, word) to be used by `flyspell-do-correct'."
  (let* ((corrects   (if flyspell-sort-corrections
                         (sort candidates 'string<)
                       candidates))
         (cor-menu   (if (consp corrects)
                         (mapcar (lambda (correct)
                                   (list correct correct))
                                 corrects)
                       '()))
         (base-menu  (let ((save `(("Save word" (save . ,word))
                                   ("Accept (session)" (session . ,word))
                                   ("Accept (buffer)" (buffer . ,word))
                                   ("Skip" (skip . ,word))
                                   ("Stop" (stop . ,word)))))
                       (if (consp cor-menu)
                           (append cor-menu (cons "" save))
                         save)))
         (menu       (cons "flyspell correction menu" base-menu)))
    (car (avy-menu "*flyspell-correct-avy*"
                   (list (format "%s [%s]" word (or ispell-local-dictionary
                                                    ispell-dictionary))
                         menu)))))

(setq flyspell-correct-interface #'flyspell-correct-avy-menu)

(provide 'flyspell-correct-avy-menu)

;;; flyspell-correct-avy-menu.el ends here
