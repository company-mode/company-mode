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
                    (down-list 1)
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
    (when (string-match ".*$" doc)
      (match-string 0 doc))))

(defun company-elisp (command &optional arg &rest ignored)
  (case command
    ('prefix (and (eq major-mode 'emacs-lisp-mode)
                  (company-grab-lisp-symbol)))
    ('candidates (let ((completion-ignore-case nil))
                   (append (all-completions arg (company-elisp-parse-let))
                           (all-completions arg obarray
                                            'company-elisp-predicate))))
    ('meta (company-elisp-doc arg))))

(provide 'company-elisp)
;;; company-elisp.el ends here
