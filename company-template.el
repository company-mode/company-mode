;;; company-template.el --- utility library for template expansion

;; Copyright (C) 2009, 2010, 2014-2017 Free Software Foundation, Inc.

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

;;; Code:

(require 'cl-lib)

(defface company-template-field
  '((((background dark)) (:background "yellow" :foreground "black"))
    (((background light)) (:background "orange" :foreground "black")))
  "Face used for editable text in template fields."
  :group 'company)

(defvar company-template-nav-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap [tab] 'company-template-forward-field)
    (define-key keymap (kbd "TAB") 'company-template-forward-field)
    (define-key keymap (kbd "C-d") 'company-template-clear-field)
    keymap))

(defvar-local company-template--buffer-templates nil)

;; interactive ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun company-template-templates-at (pos)
  (let (os)
    (dolist (o (overlays-at pos))
      ;; FIXME: Always return the whole list of templates?
      ;; We remove templates not at point after every command.
      (when (memq o company-template--buffer-templates)
        (push o os)))
    os))

(defun company-template-move-to-first (templ)
  (interactive)
  (goto-char (overlay-start templ))
  (company-template-forward-field))

(defun company-template-forward-field ()
  (interactive)
  (let ((start (point))
        (next-field-start (company-template-find-next-field)))
    (push-mark)
    (goto-char next-field-start)
    (company-template-remove-field (company-template-field-at start))))

(defun company-template-clear-field ()
  "Clear the field at point."
  (interactive)
  (company-template-remove-field (company-template-field-at (point)) t))

(defun company-template--clear-c-like-field (start end)
  "Clear the field at point for c like templates.
If there is a next field, delete everything up to it.  If the field at
point is the last field, remove preceding comma if present.  START and END
are the bounds of the current field."
  (let* ((next-field-start (company-template-find-next-field))
         (last-field-p (not (company-template-field-at next-field-start))))
    (cond ((and (not last-field-p)
                (< end next-field-start)
                (string-match "^[ ]*,+[ ]*$" (buffer-substring-no-properties
                                              end next-field-start)))
           (delete-region start next-field-start))
          ((and last-field-p
                (looking-back ",+[ ]*" (line-beginning-position)))
           (delete-region (match-beginning 0) end))
          (t (delete-region start end)))))

(defun company-template-find-next-field ()
  (let* ((start (point))
         (templates (company-template-templates-at start))
         (minimum (apply 'max (mapcar 'overlay-end templates)))
         (fields (cl-loop for templ in templates
                          append (overlay-get templ 'company-template-fields))))
    (dolist (pos (mapcar 'overlay-start fields) minimum)
      (and pos
           (> pos start)
           (< pos minimum)
           (setq minimum pos)))))

(defun company-template-field-at (&optional point)
  (cl-loop for ovl in (overlays-at (or point (point)))
           when (overlay-get ovl 'company-template-parent)
           return ovl))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun company-template-declare-template (beg end)
  (let ((ov (make-overlay beg end)))
    ;; (overlay-put ov 'face 'highlight)
    (overlay-put ov 'keymap company-template-nav-map)
    (overlay-put ov 'priority 101)
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

(defun company-template-add-field (templ beg end &optional display clear-function)
  "Add new field to template TEMPL spanning from BEG to END.
When DISPLAY is non-nil, set the respective property on the overlay.
Leave point at the end of the field.
CLEAR-FUNCTION is a function that can be used to apply custom behavior when
deleting a field in `company-template-remove-field'.  If nil,
`delete-region' is used."
  (cl-assert templ)
  (when (> end (overlay-end templ))
    (move-overlay templ (overlay-start templ) end))
  (let ((ov (make-overlay beg end))
        (siblings (overlay-get templ 'company-template-fields)))
    ;; (overlay-put ov 'evaporate t)
    (overlay-put ov 'intangible t)
    (overlay-put ov 'face 'company-template-field)
    (when display
      (overlay-put ov 'display display))
    (overlay-put ov 'company-template-parent templ)
    (overlay-put ov 'insert-in-front-hooks '(company-template-insert-hook))
    (when clear-function
      (overlay-put ov 'clear-function clear-function))
    (push ov siblings)
    (overlay-put templ 'company-template-fields siblings)))

(defun company-template-remove-field (ovl &optional clear)
  (when (overlayp ovl)
    (when (overlay-buffer ovl)
      (when clear
        (let ((clear-function (overlay-get ovl 'clear-function))
              (start (overlay-start ovl))
              (end (overlay-end ovl)))
          (if (and (not (eq clear 'clear-bounds-only))
                   (functionp clear-function))
              (funcall clear-function start end)
            (delete-region start end))))
      (delete-overlay ovl))
    (let* ((templ (overlay-get ovl 'company-template-parent))
           (siblings (overlay-get templ 'company-template-fields)))
      (setq siblings (delq ovl siblings))
      (overlay-put templ 'company-template-fields siblings))))

(defun company-template-clean-up (&optional pos)
  "Clean up all templates that don't contain POS."
  (let ((local-ovs (overlays-at (or pos (point)))))
    (dolist (templ company-template--buffer-templates)
      (unless (memq templ local-ovs)
        (company-template-remove-template templ)))))

;; hooks ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun company-template-insert-hook (ovl after-p &rest _ignore)
  "Called when a snippet input prompt is modified."
  (unless after-p
    (company-template-remove-field ovl 'clear-bounds-only)))

(defun company-template-post-command ()
  (company-template-clean-up)
  (unless company-template--buffer-templates
    (remove-hook 'post-command-hook 'company-template-post-command t)))

;; common ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun company-template-c-like-templatify (call)
  (let* ((end (point-marker))
         (beg (- (point) (length call)))
         (templ (company-template-declare-template beg end))
         paren-open paren-close)
    (with-syntax-table (make-syntax-table (syntax-table))
      (modify-syntax-entry ?< "(")
      (modify-syntax-entry ?> ")")
      (when (search-backward ")" beg t)
        (setq paren-close (point-marker))
        (forward-char 1)
        (delete-region (point) end)
        (backward-sexp)
        (forward-char 1)
        (setq paren-open (point-marker)))
      (when (search-backward ">" beg t)
        (let ((angle-close (point-marker)))
          (forward-char 1)
          (backward-sexp)
          (forward-char)
          (company-template--c-like-args templ angle-close)))
      (when (looking-back "\\((\\*)\\)(" (line-beginning-position))
        (delete-region (match-beginning 1) (match-end 1)))
      (when paren-open
        (goto-char paren-open)
        (company-template--c-like-args templ paren-close)))
    (if (overlay-get templ 'company-template-fields)
        (company-template-move-to-first templ)
      (company-template-remove-template templ)
      (goto-char end))))

(defun company-template--c-like-args (templ end)
  (let ((last-pos (point)))
    (while (re-search-forward "\\([^,]+\\),?" end 'move)
      (when (zerop (car (parse-partial-sexp last-pos (point))))
        (company-template-add-field templ last-pos (match-end 1) nil
                                    #'company-template--clear-c-like-field)
        (skip-chars-forward " ")
        (setq last-pos (point))))))

;; objc ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun company-template-objc-templatify (selector)
  (let* ((end (point-marker))
         (beg (- (point) (length selector) 1))
         (templ (company-template-declare-template beg end))
         (cnt 0))
    (save-excursion
      (goto-char beg)
      (catch 'stop
        (while (search-forward ":" end t)
          (if (looking-at "\\(([^)]*)\\) ?")
              (company-template-add-field templ (point) (match-end 1))
            ;; Not sure which conditions this case manifests under, but
            ;; apparently it did before, when I wrote the first test for this
            ;; function.  FIXME: Revisit it.
            (company-template-add-field templ (point)
                                        (progn
                                          (insert (format "arg%d" cnt))
                                          (point)))
            (when (< (point) end)
              (insert " "))
            (cl-incf cnt))
          (when (>= (point) end)
            (throw 'stop t)))))
    (company-template-move-to-first templ)))

(provide 'company-template)
;;; company-template.el ends here
