;;; company-eclim.el --- A company-mode completion back-end for eclim.

;; Copyright (C) 2009, 2011, 2013  Free Software Foundation, Inc.

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
;; Eclim version 1.7.13 or newer (?) is required.
;;
;; This completion backend is pretty barebone.
;;
;; `emacs-eclim' provides an alternative backend, and it also allows you to
;; actually control Eclim from Emacs.

;;; Code:

(require 'company)
(require 'company-template)
(eval-when-compile (require 'cl))

(defun company-eclim-executable-find ()
  (let (file)
    (dolist (eclipse-root '("/Applications/eclipse" "/usr/lib/eclipse"
                            "/usr/local/lib/eclipse"))
      (and (file-exists-p (setq file (expand-file-name "plugins" eclipse-root)))
           (setq file (car (last (directory-files file t "^org.eclim_"))))
           (file-exists-p (setq file (expand-file-name "bin/eclim" file)))
           (return file)))))

(defcustom company-eclim-executable
  (or (executable-find "eclim") (company-eclim-executable-find))
  "Location of eclim executable."
  :group 'company
  :type 'file)

(defcustom company-eclim-auto-save t
  "Determines whether to save the buffer when retrieving completions.
eclim can only complete correctly when the buffer has been saved."
  :group 'company
  :type '(choice (const :tag "Off" nil)
                 (const :tag "On" t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar company-eclim--project-dir 'unknown)
(make-variable-buffer-local 'company-eclim--project-dir)

(defvar company-eclim--project-name nil)
(make-variable-buffer-local 'company-eclim--project-name)

(defvar company-eclim--doc nil)
(make-variable-buffer-local 'company-eclim--doc)

(defun company-eclim--call-process (&rest args)
  (let ((coding-system-for-read 'utf-8)
        res)
    (require 'json)
    (with-temp-buffer
      (if (= 0 (setq res (apply 'call-process company-eclim-executable nil t nil
                                "-command" args)))
          (let ((json-array-type 'list))
            (goto-char (point-min))
            (unless (eobp)
              (json-read)))
        (message "Company-eclim command failed with error %d:\n%s" res
                 (buffer-substring (point-min) (point-max)))
        nil))))

(defun company-eclim--project-list ()
  (company-eclim--call-process "project_list"))

(defun company-eclim--project-dir ()
  (if (eq company-eclim--project-dir 'unknown)
      (setq company-eclim--project-dir
            (directory-file-name
             (expand-file-name
              (company-locate-dominating-file buffer-file-name ".project"))))
    company-eclim--project-dir))

(defun company-eclim--project-name ()
  (or company-eclim--project-name
      (let ((dir (company-eclim--project-dir)))
        (when dir
          (setq company-eclim--project-name
                (let ((project (loop for project in (company-eclim--project-list)
                                     when (equal (cdr (assoc 'path project)) dir)
                                     return project)))
                  (when project
                    (cdr (assoc 'name project)))))))))

(defun company-eclim--candidates (prefix)
  (interactive "d")
  (let ((project-file (file-relative-name buffer-file-name
                                          (company-eclim--project-dir)))
        (project-name (company-eclim--project-name)))
    (when company-eclim-auto-save
      (when (buffer-modified-p)
        (basic-save-buffer))
      ;; FIXME: Sometimes this isn't finished when we complete.
      (company-eclim--call-process "java_src_update"
                                   "-p" (company-eclim--project-name)
                                   "-f" project-file))
    (setq company-eclim--doc
          (make-hash-table :test 'equal))
    (dolist (item (cdr (assoc 'completions
                              (company-eclim--call-process
                               "java_complete" "-p" (company-eclim--project-name)
                               "-f" project-file
                               "-o" (number-to-string (1- (point)))
                               "-e" "utf-8"
                               "-l" "standard"))))
      (let* ((meta (cdr (assoc 'info item)))
             (completion meta))
        (when (string-match " [:-]" completion)
          (setq completion (substring completion 0 (match-beginning 0))))
        (puthash completion meta company-eclim--doc))))
  (let ((completion-ignore-case nil))
    (all-completions prefix company-eclim--doc)))

(defun company-eclim--meta (candidate)
  (gethash candidate company-eclim--doc))

(defun company-eclim--templatify (call)
  (let* ((end (point))
         (beg (- (point) (length call)))
         (templ (company-template-declare-template beg end)))
    (save-excursion
      (goto-char beg)
      (while (re-search-forward "\\([(,] ?\\)\\([^ ]+ \\)\\([^ ,)]*\\)" end t)
        (let ((name (match-string 3)))
          (replace-match "\\1" t)
          (decf end (length (match-string 2)))
          (company-template-add-field templ (point) name))))
    (company-template-move-to-first templ)))

(defun company-eclim (command &optional arg &rest ignored)
  "A `company-mode' completion back-end for eclim.
eclim provides access to Eclipse Java IDE features for other editors.

Completions only work correctly when the buffer has been saved.
`company-eclim-auto-save' determines whether to do this automatically."
  (interactive (list 'interactive))
  (case command
    (interactive (company-begin-backend 'company-eclim))
    (prefix (and (derived-mode-p 'java-mode 'jde-mode)
                 buffer-file-name
                 company-eclim-executable
                 (company-eclim--project-name)
                 (not (company-in-string-or-comment))
                 (or (company-grab-symbol) 'stop)))
    (candidates (company-eclim--candidates arg))
    (meta (company-eclim--meta arg))
    ;; because "" doesn't return everything
    (no-cache (equal arg ""))
    (crop (when (string-match "(" arg)
            (substring arg 0 (match-beginning 0))))
    (post-completion (when (string-match "([^)]" arg)
                       (company-eclim--templatify arg)))))

(provide 'company-eclim)
;;; company-eclim.el ends here
