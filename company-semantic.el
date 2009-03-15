(require 'company)
(require 'semantic-ia)
(eval-when-compile (require 'cl))

(defcustom company-semantic-metadata-function 'company-semantic-summary-and-doc
  "*"
  :group 'company
  :type 'function)

(defvar company-semantic-context-regexp
  "\\(->\\|\\.\\|\\_<\\)\\(\\(\\s_\\|\\sw\\)+\\_>\\=\\)")

(defun company-semantic-doc-or-summary (tag)
  (or (semantic-documentation-for-tag tag)
      (funcall semantic-idle-summary-function tag nil t)))

(defun company-semantic-summary-and-doc (tag)
  (let ((doc (semantic-documentation-for-tag tag))
        (summary (funcall semantic-idle-summary-function tag nil t)))
    (and (stringp doc)
         (string-match "\n*\\(.*\\)$" doc)
         (setq doc (match-string 1 doc)))
    (concat (funcall semantic-idle-summary-function tag nil t)
            (when doc
                  (if (< (+ (length doc) (length summary) 4) (window-width))
                      " -- "
                    "\n"))
            doc)))

(defsubst company-semantic-completions (prefix)
  (ignore-errors
    (let ((completion-ignore-case nil)
          (context (semantic-analyze-current-context)))
      (all-completions prefix (semantic-ia-get-completions context (point))))))

(defun company-semantic (command &optional arg &rest ignored)
  (case command
    ('prefix (and (memq major-mode '(c-mode c++-mode jde-mode java-mode))
                  (not (company-in-string-or-comment))
                  (or (company-grab company-semantic-context-regexp 2) "")))
    ('candidates (or (company-semantic-completions arg)
                     (mapcar 'semantic-tag-name
                             (semantic-analyze-find-tags-by-prefix arg))))
    ('meta (funcall company-semantic-metadata-function
                    (semantic-analyze-find-tag arg)))))

(provide 'company-semantic)
;;; company-semantic.el ends here
