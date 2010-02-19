(eval-when-compile (require 'cl))

(defface company-template-field
  '((((background dark)) (:background "yellow" :foreground "black"))
    (((background light)) (:background "orange" :foreground "black")))
  "*Face used for editable text in template fields."
  :group 'company)

(defvar company-template-nav-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap [remap forward-word] 'company-template-forward-field)
    (define-key keymap [remap subword-forward] 'company-template-forward-field)
    ;; M-n
    keymap))

;; interactive ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defsubst company-template-templates-at (pos)
  (let (os)
    (dolist (o (overlays-at pos))
      (when (overlay-get o 'company-template-fields)
        (push o os)))
    os))

(defun company-template-move-to-first (templ)
  (interactive)
  (let ((fields (overlay-get templ 'company-template-fields)))
    (push-mark)
    (goto-char (apply 'min (mapcar 'overlay-start fields)))))

(defun company-template-forward-field ()
  (interactive)
  (let* ((templates (company-template-templates-at (point)))
         (minimum (apply 'max (mapcar 'overlay-end templates)))
         (fields (apply 'append
                        (mapcar (lambda (templ)
                                  (overlay-get templ 'company-template-fields))
                                templates))))
    (dolist (pos (mapcar 'overlay-start fields))
      (and pos
           (> pos (point))
           (< pos minimum)
           (setq minimum pos)))
    (push-mark)
    (goto-char minimum)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar company-template--buffer-templates nil)
(make-variable-buffer-local 'company-template--buffer-templates)

(defun company-template-declare-template (beg end)
  (let ((ov (make-overlay beg end)))
    ;; (overlay-put ov 'face 'highlight)
    (overlay-put ov 'keymap company-template-nav-map)
    (overlay-put ov 'evaporate t)
    (push ov company-template--buffer-templates)
    (add-hook 'post-command-hook 'company-template-post-command nil t)
    ov))

(defun company-template-remove-template (templ)
  (mapc 'company-template-remove-field
        (overlay-get templ 'company-template-fields))
  (setq company-template--buffer-templates
        (delq templ company-template--buffer-templates))
  (delete-overlay templ))

(defun company-template-add-field (templ pos text)
  (assert templ)
  (save-excursion
    ;; (goto-char pos)
    (let ((ov (make-overlay pos pos))
          (siblings (overlay-get templ 'company-template-fields))
          (label (propertize text 'face 'company-template-field
                             'company-template-parent templ)))
      (overlay-put ov 'face 'highlight)
      (add-text-properties 0 1 '(cursor t) label)
      (overlay-put ov 'after-string label)
      ;; (overlay-put ov 'evaporate t)
      (overlay-put ov 'intangible t)
      (overlay-put ov 'company-template-parent templ)
      (overlay-put ov 'insert-in-front-hooks '(company-template-remove))
      (push ov siblings)
      (overlay-put templ 'company-template-fields siblings))))

(defun company-template-remove-field (field)
  (when (overlayp field)
    ;; (delete-region (overlay-start field) (overlay-end field))
    (delete-overlay field))
  ;; TODO: unlink
  )

(defun company-template-clean-up (&optional pos)
  "Clean up all templates that don't contain POS."
  (unless pos (setq pos (point)))
  (let ((local-ovs (overlays-in (- pos 2) pos)))
    (dolist (templ company-template--buffer-templates)
      (unless (memq templ local-ovs)
        (company-template-remove-template templ)))))

;; hooks ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun company-template-remove (overlay after-p beg end &optional r)
  "Called when a snippet input prompt is modified."
  (when after-p
    (delete-overlay overlay)))

(defun company-template-post-command ()
  (company-template-clean-up)
  (unless company-template--buffer-templates
    (remove-hook 'post-command-hook 'company-template-post-command t)))

(provide 'company-template)
;;; company-template.el ends here
