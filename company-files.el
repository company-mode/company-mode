;;; company-files.el --- company-mode completion back-end for file names

;; Copyright (C) 2009-2011, 2013  Free Software Foundation, Inc.

;; Author: Nikolaj Schumacher

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


;;; Commentary:
;;

;;; Code:

(require 'company)
(require 'cl-lib)

(defun company-files-directory-files (dir prefix)
  (ignore-errors
    (if (equal prefix "")
        (directory-files dir nil "\\`[^.]\\|\\`.[^.]")
      (file-name-all-completions prefix dir))))

(defvar company-files-regexps
  (let* ((begin (if (eq system-type 'windows-nt)
                   "[[:alpha:]]:/"
		  "/"))
	 (begin (concat "\\(?:\\.\\{1,2\\}/\\|~/\\|" begin "\\)")))
    (list (concat "\"\\(" begin "[^\"\n]*\\)")
          (concat "\'\\(" begin "[^\'\n]*\\)")
          (concat "\\(?:[ \t]\\|^\\)\\(" begin "[^ \t\n]*\\)"))))

(defun company-files-grab-existing-name ()
  ;; Grab file names with spaces, only when they include quotes.
  (let (file dir)
    (and (cl-dolist (regexp company-files-regexps)
           (when (setq file (company-grab-line regexp 1))
             (cl-return file)))
         (setq dir (file-name-directory file))
         (not (string-match "//" dir))
         (file-exists-p dir)
         (file-name-all-completions (file-name-nondirectory file) dir)
         file)))

(defvar company-files-completion-cache nil)

(defun company-files-complete (prefix)
  (let* ((dir (file-name-directory prefix))
         (cf-completion-cache-id (concat (expand-file-name dir)
                                           (file-name-nondirectory prefix)
                                           (mapconcat  'number-to-string
                                                       (nth 5 (file-attributes dir))
                                                       "")))
         (file (file-name-nondirectory prefix))
         candidates directories)
    (unless (equal cf-completion-cache-id (car company-files-completion-cache))
      (dolist (file (company-files-directory-files dir file))
        (setq file (concat dir file))
        (push file candidates)
        (when (file-directory-p file)
          (push file directories)))
      (dolist (directory (reverse directories))
        ;; Add one level of children.
        (dolist (child (company-files-directory-files directory ""))
          (push (concat directory
                        (unless (eq (aref directory (1- (length directory))) ?/) "/")
                        child) candidates)))
      (setq company-files-completion-cache  (cons cf-completion-cache-id (nreverse candidates))))
    (all-completions prefix
                     (cdr company-files-completion-cache))))

;;;###autoload
(defun company-files (command &optional arg &rest ignored)
  "`company-mode' completion back-end existing file names.
Completions are triggered when the text inside quotes starts with any of 
the following: ./, ../, ~/, or root drive"
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-files))
    (prefix (company-files-grab-existing-name))
    (candidates (company-files-complete arg))
    (location (cons (dired-noselect
                     (file-name-directory (directory-file-name arg))) 1))
    (sorted t)
    (no-cache t)))

(provide 'company-files)
;;; company-files.el ends here
