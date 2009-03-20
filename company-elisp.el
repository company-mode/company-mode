;;; company-elisp.el --- a company-mode completion back-end for emacs-lisp-mode
;;
;; Copyright (C) 2009 Nikolaj Schumacher
;;
;; This file is part of company.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(require 'company)
(eval-when-compile (require 'cl))

(defvar company-lisp-symbol-regexp
  "\\_<\\(\\sw\\|\\s_\\)+\\_>\\=")

(defun company-grab-lisp-symbol ()
  (let ((prefix (or (company-grab company-lisp-symbol-regexp) "")))
    (unless (and (company-in-string-or-comment (- (point) (length prefix)))
                 (/= (char-before (- (point) (length prefix))) ?`))
      prefix)))

(defun company-elisp-predicate (symbol)
  (or (boundp symbol)
      (fboundp symbol)))

(defvar company-elisp-parse-limit 30)
(defvar company-elisp-parse-depth 100)

(defun company-elisp-parse-let ()
  (let (vars)
    (ignore-errors
      (save-excursion
        (dotimes (i company-elisp-parse-depth)
          (up-list -1)
          (save-excursion
            (when (looking-at "([ \t\n]*let")
              (down-list 2)
              (ignore-errors
                (dotimes (i company-elisp-parse-limit)
                  (save-excursion
                    (when (looking-at "[ \t\n]*(")
                      (down-list 1))
                    (if (looking-at "[ \t\n]*\\(\\(?:\\sw\\|\\s_\\)+\\)")
                        (add-to-list 'vars (match-string-no-properties 1))
                      (error)))
                  (forward-sexp))))))))
    vars))

(defun company-elisp-doc (symbol)
  (let* ((symbol (intern symbol))
         (doc (if (fboundp symbol)
                  (documentation symbol t)
                (documentation-property symbol 'variable-documentation t))))
    (and (stringp doc)
         (string-match ".*$" doc)
         (match-string 0 doc))))

(defun company-elisp (command &optional arg &rest ignored)
  "A `company-mode' completion back-end for `emacs-lisp-mode'."
  (case command
    ('prefix (and (eq (derived-mode-p 'emacs-lisp-mode) 'emacs-lisp-mode)
                  (company-grab-lisp-symbol)))
    ('candidates (let ((completion-ignore-case nil))
                   (append (all-completions arg (company-elisp-parse-let))
                           (all-completions arg obarray
                                            'company-elisp-predicate))))
    ('meta (company-elisp-doc arg))
    ('doc-buffer (describe-function 'describe-function)
                 (help-buffer))))

(provide 'company-elisp)
;;; company-elisp.el ends here
