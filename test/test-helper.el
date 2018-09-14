(require 'cl)
(require 'el-mock)
(require 'undercover)
(require 'subr-x)

(setq user-home-directory (concat (getenv "HOME") "/"))

(undercover "*.el")

(add-to-list 'load-path ".")
(load "flyspell-correct.el")

(defmacro with-no-mistakes (&rest body)
  `(with-flyspell-buffer
    "|The licenses for most software and other practical works
are designed to take away your freedom to share and change the
works. By contrast, the GNU General Public License is intended to
guarantee your freedom to share and change all versions of a
program--to make sure it remains free software for all its users.
We, the Free Software Foundation, use the GNU General Public
License for most of our software; it applies also to any other
work released this way by its authors. You can apply it to your
programs, too." ,@body))

(defmacro with-mistake-in-the-middle|cursor-beginning (&rest body)
  `(with-flyspell-buffer
    "The licenses for most software and other practical works are
designed to take away your freedom to share and change the works.
By contrast, the GNU General Public License is intended to
guarantee your freedom to share and change all |versiuns of a
program--to make sure it remains free software for all its users.
We, the Free Software Foundation, use the GNU General Public
License for most of our software; it applies also to any other
work released this way by its authors. You can apply it to your
programs, too." ,@body))

(defmacro with-mistake-in-the-middle|cursor-before (&rest body)
  `(with-flyspell-buffer
    "The licenses for most software and other practical works are
designed to take away your freedom to share and change the works.
By contrast, the GNU General Public License |is intended to
guarantee your freedom to share and change all versiuns of a
program--to make sure it remains free software for all its users.
We, the Free Software Foundation, use the GNU General Public
License for most of our software; it applies also to any other
work released this way by its authors. You can apply it to your
programs, too." ,@body))

(defmacro with-mistake-in-the-middle|cursor-inside (&rest body)
  `(with-flyspell-buffer
    "The licenses for most software and other practical works are
designed to take away your freedom to share and change the works.
By contrast, the GNU General Public License is intended to
guarantee your freedom to share and change all ver|siuns of a
program--to make sure it remains free software for all its users.
We, the Free Software Foundation, use the GNU General Public
License for most of our software; it applies also to any other
work released this way by its authors. You can apply it to your
programs, too." ,@body))

(defmacro with-mistake-in-the-middle|cursor-end (&rest body)
  `(with-flyspell-buffer
    "The licenses for most software and other practical works are
designed to take away your freedom to share and change the works.
By contrast, the GNU General Public License is intended to
guarantee your freedom to share and change all versiuns| of a
program--to make sure it remains free software for all its users.
We, the Free Software Foundation, use the GNU General Public
License for most of our software; it applies also to any other
work released this way by its authors. You can apply it to your
programs, too." ,@body))

(defmacro with-mistake-in-the-middle|cursor-after (&rest body)
  `(with-flyspell-buffer
    "The licenses for most software and other practical works are
designed to take away your freedom to share and change the works.
By contrast, the GNU General Public License is intended to
guarantee your freedom to share and change all versiuns of a
program--to make sure it remains free software for all| its
users. We, the Free Software Foundation, use the GNU General
Public License for most of our software; it applies also to any
other work released this way by its authors. You can apply it to
your programs, too." ,@body))

(defmacro with-flyspell-buffer (text &rest body)
  `(with-temp-buffer
     (insert ,text)
     (let ((flyspell-correct-interface #'correct)
           (ispell-dictionary-alist
            '((nil "[A-Za-z]" "[^A-Za-z]" "[']" nil ("-B") nil iso-8859-1)
              ("english" "[A-Za-z]" "[^A-Za-z]" "[']" nil ("-B") nil iso-8859-1)))
           (ispell-current-dictionary "english"))
       (flyspell-buffer)
       (sync-cursor)
       (with-mock
         (mock (window-start) => (buffer-end 0))
         (mock (window-end) => (buffer-end 1))
         ,@body))))

(defmacro ensure-no-corrections ()
  `(not-called correct))

(defmacro ensure-correction (from to)
  `(mock (correct * ,from) => ,to :times 1))

(defun sync-cursor ()
  (interactive)
  (beginning-of-buffer)
  (let ((cursor "|"))
    (search-forward cursor)
    (delete-backward-char (length cursor))))

(defun correct (word candidates))
