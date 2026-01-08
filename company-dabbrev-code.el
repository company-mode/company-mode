;;; company-dabbrev-code.el --- dabbrev-like company-mode backend for code  -*- lexical-binding: t -*-

;; Copyright (C) 2009-2011, 2013-2016, 2021-2025  Free Software Foundation, Inc.

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
(require 'company-dabbrev)
(require 'cl-lib)

(defgroup company-dabbrev-code nil
  "dabbrev-like completion backend for code."
  :group 'company)

(defcustom company-dabbrev-code-modes
  '(prog-mode
    batch-file-mode csharp-mode css-mode erlang-mode haskell-mode jde-mode
    lua-mode python-mode)
  "Modes that use `company-dabbrev-code'.
In all these modes (and their derivatives) `company-dabbrev-code' will
complete only symbols, not text in comments or strings.  In other modes
`company-dabbrev-code' will pass control to other backends
\(e.g. `company-dabbrev'\).  Value t means complete in all modes."
  :type '(choice (repeat :tag "Some modes" (symbol :tag "Major mode"))
                 (const :tag "All modes" t)))

(defcustom company-dabbrev-code-other-buffers t
  "Determines whether `company-dabbrev-code' should search other buffers.
If `all', search all other buffers, except the ignored ones.  If t, search
buffers with the same major mode.  If `code', search all
buffers with major modes in `company-dabbrev-code-modes', or derived from one of
them.  This can also be a function that takes the current buffer as
parameter and returns a list of major modes to search.  See also
`company-dabbrev-code-time-limit'."
  :type '(choice (const :tag "Off" nil)
                 (const :tag "Same major mode" t)
                 (const :tag "Code major modes" code)
                 (const :tag "All" all)
                 (function :tag "Function to return similar major-modes" group)))

(defcustom company-dabbrev-code-time-limit .1
  "Determines how long `company-dabbrev-code' should look for matches."
  :type '(choice (const :tag "Off" nil)
                 (number :tag "Seconds")))

(defcustom company-dabbrev-code-everywhere nil
  "Non-nil to offer completions in comments and strings."
  :type 'boolean)

(defcustom company-dabbrev-code-ignore-case nil
  "Non-nil to ignore case when collecting completion candidates."
  :type 'boolean)

(defcustom company-dabbrev-code-completion-styles nil
  "Non-nil to use the completion styles for fuzzy matching."
  :type '(choice (const :tag "Prefix matching only" nil)
                 (const :tag "Matching according to `completion-styles'" t)
                 (list :tag "Custom list of styles" symbol))
  :package-version '(company . "1.0.0"))

(defvar-local company-dabbrev--boundaries nil)
(defvar-local company-dabbrev-code--sorted nil)

(defun company-dabbrev-code--make-regexp (prefix)
  (let ((prefix-re
         (cond
          ((string-empty-p prefix)
           "\\([a-zA-Z]\\|\\s_\\)")
          ((not company-dabbrev-code-completion-styles)
           (regexp-quote prefix))
          (t
           ;; Use the cache at least after 2 chars.  We could also cache
           ;; earlier, for users who set company-min-p-l to 1 or 0.
           (let ((prefix (if (>= (length prefix) 2)
                             (substring prefix 0 2)
                           prefix)))
             (concat
              "\\(\\sw\\|\\s_\\)*"
              (mapconcat #'regexp-quote
                         (mapcar #'string prefix)
                         "\\(\\sw\\|\\s_\\)*")))))))
    (concat "\\_<" prefix-re "\\(\\sw\\|\\s_\\)*\\_>")))

;;;###autoload
(defun company-dabbrev-code (command &optional arg &rest rest)
  "dabbrev-like `company-mode' backend for code.
The backend looks for all symbols in the current buffer that aren't in
comments or strings."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-dabbrev-code))
    (prefix (and (or (eq t company-dabbrev-code-modes)
                     (cl-some #'derived-mode-p company-dabbrev-code-modes))
                 (or company-dabbrev-code-everywhere
                     (not (company-in-string-or-comment)))
                 (company-grab-symbol-parts)))
    (candidates (company-dabbrev--candidates arg (car rest)))
    (adjust-boundaries (and company-dabbrev-code-completion-styles
                            (company--capf-boundaries
                             company-dabbrev--boundaries)))
    (expand-common (company-dabbrev-code--expand-common arg (car rest)))
    (kind 'text)
    (sorted company-dabbrev-code--sorted)
    (no-cache t)
    (ignore-case company-dabbrev-code-ignore-case)
    (match (when company-dabbrev-code-completion-styles
             (company--match-from-capf-face arg)))
    (duplicates t)))

(defun company-dabbrev-code--expand-common (prefix suffix)
  (when company-dabbrev-code-completion-styles
    (let ((completion-styles (if (listp company-dabbrev-code-completion-styles)
                                 company-dabbrev-code-completion-styles
                               completion-styles)))
      (company--capf-expand-common prefix suffix
                                   (company-dabbrev-code--table prefix)))))

(defun company-dabbrev--candidates (prefix suffix)
  (let* ((case-fold-search company-dabbrev-code-ignore-case))
    (company-dabbrev-code--filter
     prefix suffix
     (company-dabbrev-code--table prefix))))

(defun company-dabbrev-code--table (prefix)
  (let ((regexp (company-dabbrev-code--make-regexp prefix)))
    (company-cache-fetch
     'dabbrev-code-candidates
     (lambda ()
       (company-dabbrev--search
        regexp
        company-dabbrev-code-time-limit
        (pcase company-dabbrev-code-other-buffers
          (`t (list major-mode))
          (`code company-dabbrev-code-modes)
          (`all `all)
          ((pred functionp) (funcall company-dabbrev-code-other-buffers (current-buffer))))
        (not company-dabbrev-code-everywhere)))
     :expire t
     :check-tag
     (cons regexp company-dabbrev-code-completion-styles))))

(defun company-dabbrev-code--filter (prefix suffix table)
  (let ((completion-ignore-case company-dabbrev-code-ignore-case)
        (completion-styles (if (listp company-dabbrev-code-completion-styles)
                               company-dabbrev-code-completion-styles
                             completion-styles))
        (metadata (completion-metadata prefix table nil))
        res)
    (if (not company-dabbrev-code-completion-styles)
        (all-completions prefix table)
      (setq res (company--capf-completions
                 prefix suffix
                 table nil
                 metadata))
      (when-let* ((sort-fn (completion-metadata-get metadata 'display-sort-function)))
        (setq company-dabbrev-code--sorted t)
        (setf (alist-get :completions res)
              (funcall sort-fn (alist-get :completions res))))
      (setq company-dabbrev--boundaries
            (company--capf-boundaries-markers
             (assoc-default :boundaries res)
             company-dabbrev--boundaries))
      (assoc-default :completions res))))

(provide 'company-dabbrev-code)
;;; company-dabbrev-code.el ends here
