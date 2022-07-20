;;; company-gtags.el --- company-mode completion backend for GNU Global

;; Copyright (C) 2009-2011, 2013-2021  Free Software Foundation, Inc.

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
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:
;;

;;; Code:

(require 'company)
(require 'company-template)
(require 'cl-lib)

(defgroup company-gtags nil
  "Completion backend for GNU Global."
  :group 'company)

(define-obsolete-variable-alias
  'company-gtags-gnu-global-program-name
  'company-gtags-executable "earlier")

(defcustom company-gtags-executable
  (executable-find "global")
  "Location of GNU global executable."
  :type 'string)

(defcustom company-gtags-insert-arguments t
  "When non-nil, insert function arguments as a template after completion."
  :type 'boolean
  :package-version '(company . "0.8.1"))

(defvar-local company-gtags--tags-available-p 'unknown)
(defvar company-gtags--executable nil)

(defcustom company-gtags-modes '(prog-mode jde-mode)
  "Modes that use `company-gtags'.
In all these modes (and their derivatives) `company-gtags' will perform
completion."
  :type '(repeat (symbol :tag "Major mode"))
  :package-version '(company . "0.8.4"))

(defun company-gtags--tags-available-p ()
  (if (eq company-gtags--tags-available-p 'unknown)
      (setq company-gtags--tags-available-p
            (locate-dominating-file buffer-file-name "GTAGS"))
    company-gtags--tags-available-p))

;; Avoid byte-compilation warnings on Emacs < 27.
(declare-function with-connection-local-variables "files-x")
(declare-function connection-local-set-profile-variables "files-x")
(declare-function connection-local-set-profiles "files-x")

(defun company-gtags--executable ()
  (cond
   ((or (not (file-remote-p default-directory))        ;; not remote
        (local-variable-p 'company-gtags--executable)) ;; the value is already local
    company-gtags--executable)
   ((when-let* (((version<= "27" emacs-version))       ;; search remote and set connection-local
	        (criteria (connection-local-criteria-for-default-directory))
	        (symvars (intern (format "company-gtags--%s-vars"
                                         (file-remote-p default-directory))))
	        (enable-connection-local-variables t))
      (unless (alist-get symvars connection-local-profile-alist)
        (with-connection-local-variables
         (let ((gtags-executable (if (local-variable-p 'company-gtags-executable)
			             company-gtags-executable
			           (file-name-nondirectory company-gtags-executable))))
	   (connection-local-set-profile-variables
	    symvars
	    `((company-gtags--executable . ,(executable-find gtags-executable t))))
	   (connection-local-set-profiles criteria symvars))))
      (hack-connection-local-variables-apply criteria)
      t)
    company-gtags--executable)
   (t     ;; If here it means that it is an old emacs and the value es not local...
    (make-local-variable company-gtags--executable))))

(defun company-gtags--fetch-tags (prefix)
  (with-temp-buffer
    (let (tags)
      ;; For some reason Global v 6.6.3 is prone to returning exit status 1
      ;; even on successful searches when '-T' is used.
      (when (/= 3 (process-file (company-gtags--executable) nil
                               ;; "-T" goes through all the tag files listed in GTAGSLIBPATH
                               (list (current-buffer) nil) nil "-xGqT" (concat "^" prefix)))
        (goto-char (point-min))
        (cl-loop while
                 (re-search-forward (concat
                                     "^"
                                     "\\([^ ]*\\)" ;; completion
                                     "[ \t]+\\([[:digit:]]+\\)" ;; linum
                                     "[ \t]+\\([^ \t]+\\)" ;; file
                                     "[ \t]+\\(.*\\)" ;; definition
                                     "$"
                                     ) nil t)
                 collect
                 (propertize (match-string 1)
                             'meta (match-string 4)
                             'location (cons (expand-file-name (match-string 3))
                                             (string-to-number (match-string 2)))
                             ))))))

(defun company-gtags--annotation (arg)
  (let ((meta (get-text-property 0 'meta arg)))
    (when (string-match (concat (regexp-quote arg) " *(") meta)
      (with-temp-buffer
        (let ((start (match-end 0)))
          (insert meta)
          (goto-char start)
          (condition-case nil
              (forward-sexp)
            (scan-error
             (goto-char (point-max))))
          (buffer-substring-no-properties
           start (point)))))))

;;;###autoload
(defun company-gtags (command &optional arg &rest ignored)
  "`company-mode' completion backend for GNU Global."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-gtags))
    (prefix (and (company-gtags--executable)
                 buffer-file-name
                 (apply #'derived-mode-p company-gtags-modes)
                 (not (company-in-string-or-comment))
                 (company-gtags--tags-available-p)
                 (or (company-grab-symbol) 'stop)))
    (candidates (company-gtags--fetch-tags arg))
    (sorted t)
    (duplicates t)
    (annotation (company-gtags--annotation arg))
    (meta (get-text-property 0 'meta arg))
    (location (get-text-property 0 'location arg))
    (post-completion (let ((anno (company-gtags--annotation arg)))
                       (when (and company-gtags-insert-arguments anno)
                         (insert anno)
                         (company-template-c-like-templatify anno))))))

(provide 'company-gtags)
;;; company-gtags.el ends here
