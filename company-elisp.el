(require 'company)
(eval-when-compile (require 'cl))

(defvar company-lisp-symbol-regexp
  "\\_<\\(\\sw\\|\\s_\\)+\\_>\\=")

(defun company-grab-lisp-symbol ()
  (let ((prefix (or (company-grab company-lisp-symbol-regexp) "")))
    (unless (and (company-in-string-or-comment (- (point) (length prefix)))
                 (/= (char-before (- (point) (length prefix))) ?`))
      prefix)))

(defun company-elisp (command &optional arg &rest ignored)
  (case command
    ('prefix (and (eq major-mode 'emacs-lisp-mode)
                  (company-grab-lisp-symbol)))
    ('candidates (let ((completion-ignore-case nil))
                   (all-completions arg obarray
                                    (lambda (symbol) (or (boundp symbol)
                                                         (fboundp symbol))))))))

(provide 'company-elisp)
;;; company-elisp.el ends here
