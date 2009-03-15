(require 'company)
(require 'dabbrev)
(eval-when-compile (require 'cl))

(defun company-grab-dabbrev-prefix ()
  (save-excursion
    (when (looking-at "\\>")
      (let ((end (point)))
        (dabbrev--reset-global-variables)
        (dabbrev--goto-start-of-abbrev)
        (buffer-substring-no-properties (point) end)))))

(defun company-dabbrev (command &optional arg &rest ignored)
  (case command
    ('prefix (company-grab-dabbrev-prefix))
    ('candidates (let ((dabbrev-check-other-buffers))
                   (dabbrev--reset-global-variables)
                   (dabbrev--find-all-expansions arg t)))
    ('ignore-case t)))

(provide 'company-dabbrev)
;;; company-dabbrev.el ends here
