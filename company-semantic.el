(require 'company)
(require 'semantic-ia)
(eval-when-compile (require 'cl))

(defvar company-semantic-context-regexp
  "\\(->\\|\\.\\|\\_<\\)\\(\\(\\s_\\|\\sw\\)+\\_>\\=\\)")

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
                             (semantic-analyze-find-tags-by-prefix arg))))))

(provide 'company-semantic)
;;; company-semantic.el ends here
