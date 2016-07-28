;;; flyspell-correct-ido.el --- correcting words with flyspell via ido interface
;;
;; Copyright (c) 2016 Boris Buliga
;;
;; Author: Boris Buliga <d12frosted@gmail.com>
;; URL: https://github.com/d12frosted/flyspell-correct
;; Package-Version: 0.1.0
;; Package-Requires: ((flyspell-correct "0.1.0"))
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;; This package provides ido interface for flyspell-correct package.
;;
;; Points of interest are `flyspell-correct-word-generic' and
;; `flyspell-correct-previous-word-generic'.
;;
;; Example usage:
;;
;;   (require 'flyspell-correct-ido)
;;   (setq flyspell-correct-interface #'flyspell-correct-ido) ;; optional
;;   (define-key flyspell-mode-map (kbd "C-;") 'flyspell-correct-word-generic)
;;
;;; Code:
;;

;; Requires

(require 'flyspell-correct)
(require 'ido)

(defun flyspell-correct-ido (candidates word)
  "Run `ido-completing-read' for the given CANDIDATES given by flyspell for the WORD.
Return a selected word to use as a replacement or a tuple
of (command, word) to be used by `flyspell-do-correct'."
  (let* ((save "[SAVE]")
         (accept-session "[ACCEPT (session)]")
         (accept-buffer "[ACCEPT (buffer)]")
         (result (ido-completing-read
                  (format "Correcting '%s': " word)
                  (append candidates
                          (list save accept-session accept-buffer)))))
    (cond
     ((string= result save)
      (cons 'save word))
     ((string= result accept-session)
      (cons 'session word))
     ((string= result accept-buffer)
      (cons 'buffer word))
     (t
      result))))

(setq flyspell-correct-interface #'flyspell-correct-ido)

(provide 'flyspell-correct-ido)

;;; flyspell-correct-ido.el ends here
