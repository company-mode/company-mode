(require 'company)
(require 'ispell)
(eval-when-compile (require 'cl))

(defcustom company-ispell-dictionary nil
  "*Dictionary to use for `company-ispell'.
If nil, use `ispell-complete-word-dict'."
  :group 'company
  :type '(choice (const :tag "default (nil)" nil)
                 (file :tag "dictionary" t)))

(defun company-ispell (command &optional arg &rest ignored)
  (case command
    ('prefix (company-grab "\\<\\w+\\>"))
    ('candidates (lookup-words arg (or company-ispell-dictionary
                                       ispell-complete-word-dict)))
    ('sorted t)
    ('ignore-case t)))

(provide 'company-ispell)
;;; company-ispell.el ends here
