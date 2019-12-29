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
guarantee your freedom to share and change all |†versiuns of a
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
guarantee your freedom to share and change all †ver|siuns of a
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
guarantee your freedom to share and change all †versiuns| of a
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
       (let* ((points (sync-cursors "|" "†"))
              (initial-point (car points))
              (end-point (cadr points)))
         (message "points = %s" points)
         (with-mock
           (goto-char initial-point)
           (message "point before: %s" (point))
           ,@body
           (message "point after: %s" (point))
           (should (equal end-point
                          (point))))))))

(defmacro ensure-no-corrections ()
  `(not-called correct))

(defmacro ensure-correction (from to)
  `(mock (correct * ,from) => ,to :times 1))

(defun sync-cursor (cursor)
  (beginning-of-buffer)
  (ignore-errors
    (search-forward cursor)
    (delete-backward-char (length cursor))
    (point)))

(defun sync-cursors (initial-cursor end-cursor)
  (let* ((initial-point0 (sync-cursor initial-cursor))
         (end-point0 (or (sync-cursor end-cursor)
                         initial-point0))
         (initial-point initial-point0)
         (end-point end-point0))
    (when (> initial-point0 end-point0)
      (setq initial-point (- initial-point (length end-cursor))))
    (list initial-point end-point)))

(defun correct (candidates word))
