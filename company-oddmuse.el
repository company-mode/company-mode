(require 'company)
(require 'oddmuse)
(eval-when-compile (require 'cl))

(defvar company-oddmuse-link-regexp
  "\\(\\<[A-Z][[:alnum:]]*\\>\\)\\|\\[\\[\\([[:alnum:]]+\\>\\|\\)")

(defun company-oddmuse (command &optional arg &rest ignored)
  (case command
    ('prefix (let ((case-fold-search nil))
               (and (eq major-mode 'oddmuse-mode)
                    (looking-back company-oddmuse-link-regexp (point-at-bol))
                    (or (match-string 1)
                        (match-string 2)))))
    ('candidates (all-completions arg
                                  (oddmuse-make-completion-table oddmuse-wiki)))
    ))

(provide 'company-oddmuse)
;;; company-oddmuse.el ends here
