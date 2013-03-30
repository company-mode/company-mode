;;; company-elisp.el --- A company-mode completion back-end for emacs-lisp-mode -*- lexical-binding: t -*-

;; Copyright (C) 2009, 2011-2013  Free Software Foundation, Inc.

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
(eval-when-compile (require 'cl))
(require 'help-mode)
(require 'find-func)

(defcustom company-elisp-detect-function-context t
  "If enabled, offer Lisp functions only in appropriate contexts.
Functions are offered for completion only after ' and \(."
  :group 'company
  :type '(choice (const :tag "Off" nil)
                 (const :tag "On" t)))

(defcustom company-elisp-show-locals-first t
  "If enabled, locally bound variables and functions are displayed
first in the candidates list."
  :group 'company
  :type '(choice (const :tag "Off" nil)
                 (const :tag "On" t)))

(defun company-grab-lisp-symbol ()
  (let ((prefix (company-grab-symbol)))
    (if prefix
        (unless (and (company-in-string-or-comment)
                     (/= (char-before (- (point) (length prefix))) ?`))
          prefix)
      'stop)))

(defun company-elisp-predicate (symbol)
  (or (boundp symbol)
      (fboundp symbol)
      (facep symbol)
      (featurep symbol)))

(defvar company-elisp-parse-limit 30)
(defvar company-elisp-parse-depth 100)

(defvar company-elisp-var-binding-regexp
  (concat "\\_<\\(?:cl-\\)?" (regexp-opt '("let" "defun" "defmacro" "defsubst"
                                           "lambda" "lexical-let"))
          "\\*?\\_>")
  "Regular expression matching head of a multiple variable bindings form.")

(defvar company-elisp-var-binding-regexp-1
  (concat "\\_<\\(?:cl-\\)?" (regexp-opt '("dolist" "dotimes")) "\\_>")
  "Regular expression matching head of a form with one variable binding.")

(defvar company-elisp-fun-binding-regexp
  (concat "\\_<\\(?:cl-\\)?" (regexp-opt '("flet" "labels")) "\\*?\\_>")
  "Regular expression matching head of a function bindings form.")

(defun company-elisp-locals (prefix functions-p)
  (let ((regexp (concat "[ \t\n]*\\(\\_<" (regexp-quote prefix)
                        "\\(?:\\sw\\|\\s_\\)*\\_>\\)"))
        (pos (point))
        res)
    (condition-case nil
        (save-excursion
          (dotimes (i company-elisp-parse-depth)
            (up-list -1)
            (save-excursion
              (when (eq (char-after) ?\()
                (forward-char 1)
                (when (ignore-errors
                        (save-excursion (forward-list)
                                        (<= (point) pos)))
                  (skip-chars-forward " \t\n")
                  (cond
                   ((looking-at (if functions-p
                                    company-elisp-fun-binding-regexp
                                  company-elisp-var-binding-regexp))
                    (down-list 1)
                    (condition-case nil
                        (dotimes (i company-elisp-parse-limit)
                          (save-excursion
                            (when (looking-at "[ \t\n]*(")
                              (down-list 1))
                            (when (looking-at regexp)
                              (pushnew (match-string-no-properties 1) res)))
                          (forward-sexp))
                      (scan-error nil)))
                   ((unless functions-p
                      (looking-at company-elisp-var-binding-regexp-1))
                    (down-list 1)
                    (when (looking-at regexp)
                      (pushnew (match-string-no-properties 1) res)))))))))
      (scan-error nil))
    res))

(defun company-elisp-candidates (prefix)
  (let* ((predicate (company-elisp-candidates-predicate prefix))
         (locals (company-elisp-locals prefix (eq predicate 'fboundp)))
         (globals (company-elisp-globals prefix predicate))
         (locals (loop for local in locals
                       when (not (member local globals))
                       collect local)))
    (if company-elisp-show-locals-first
        (append (sort locals 'string<)
                (sort globals 'string<))
      (append locals globals))))

(defun company-elisp-globals (prefix predicate)
  (all-completions prefix obarray predicate))

(defun company-elisp-candidates-predicate (prefix)
  (let* ((completion-ignore-case nil)
         (before (char-before (- (point) (length prefix)))))
    (if (and company-elisp-detect-function-context
             (not (eq before ?')))
        (if (and (eq before ?\()
                 (not
                  (save-excursion
                    (ignore-errors
                      (up-list -2)
                      (and (save-excursion
                             (forward-char 1)
                             (looking-at "[ \t\n]*("))
                           (prog1 (search-backward "(")
                             (forward-char 1))
                           (looking-at company-elisp-var-binding-regexp))))))
            'fboundp
          'boundp)
      'company-elisp-predicate)))

(defun company-elisp-doc (symbol)
  (let* ((symbol (intern symbol))
         (doc (if (fboundp symbol)
                  (documentation symbol t)
                (documentation-property symbol 'variable-documentation t))))
    (and (stringp doc)
         (string-match ".*$" doc)
         (match-string 0 doc))))

;;;###autoload
(defun company-elisp (command &optional arg &rest ignored)
  "A `company-mode' completion back-end for `emacs-lisp-mode'."
  (interactive (list 'interactive))
  (case command
    (interactive (company-begin-backend 'company-elisp))
    (prefix (and (eq (derived-mode-p 'emacs-lisp-mode) 'emacs-lisp-mode)
                 (company-grab-lisp-symbol)))
    (candidates (company-elisp-candidates arg))
    (sorted company-elisp-show-locals-first)
    (meta (company-elisp-doc arg))
    (doc-buffer (let ((symbol (intern arg)))
                  (save-window-excursion
                    (ignore-errors
                      (cond
                       ((fboundp symbol) (describe-function symbol))
                       ((boundp symbol) (describe-variable symbol))
                       ((featurep symbol) (describe-package symbol))
                       ((facep symbol) (describe-face symbol))
                       (t (signal 'user-error nil)))
                      (help-buffer)))))
    (location (let ((sym (intern arg)))
                (cond
                 ((fboundp sym) (find-definition-noselect sym nil))
                 ((boundp sym) (find-definition-noselect sym 'defvar))
                 ((featurep sym) (cons (find-file-noselect (find-library-name
                                                            (symbol-name sym)))
                                       0))
                 ((facep sym) (find-definition-noselect sym 'defface)))))))

(provide 'company-elisp)
;;; company-elisp.el ends here
