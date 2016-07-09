;;; company-tests.el --- company-mode test helpers  -*- lexical-binding: t -*-

;; Copyright (C) 2011, 2013-2016  Free Software Foundation, Inc.

;; Author: Dmitry Gutov

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

(require 'company)

(defvar company-dir (file-name-directory (or load-file-name
                                             buffer-file-name)))

(defun company--column (&optional pos)
  (car (company--col-row pos)))

(defun company-call (name &rest args)
  (let* ((maybe (intern (format "company-%s" name)))
         (command (if (fboundp maybe) maybe name)))
    (let ((this-command command))
      (run-hooks 'pre-command-hook))
    (apply command args)
    (let ((this-command command))
      (run-hooks 'post-command-hook))))

(provide 'company-tests)
