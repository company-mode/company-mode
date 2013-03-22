;;; company-template.el

;; Copyright (C) 2009, 2010 Free Software Foundation, Inc.

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

(eval-when-compile (require 'cl))

(defface company-template-field
  '((((background dark)) (:background "yellow" :foreground "black"))
    (((background light)) (:background "orange" :foreground "black")))
  "Face used for editable text in template fields."
  :group 'company)

(defvar company-template-nav-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap [tab] 'company-template-forward-field)
    keymap))

;; interactive ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun company-template-templates-at (pos)
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
  (let* ((start (point))
         (templates (company-template-templates-at (point)))
         (minimum (apply 'max (mapcar 'overlay-end templates)))
         (fields (loop for templ in templates
                       append (overlay-get templ 'company-template-fields))))
    (dolist (pos (mapcar 'overlay-start fields))
      (and pos
           (> pos (point))
           (< pos minimum)
           (setq minimum pos)))
    (push-mark)
    (goto-char minimum)
    (let ((field (loop for ovl in (overlays-at start)
                       when (overlay-get ovl 'company-template-parent)
                       return ovl)))
      (company-template-remove-field field))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar company-template--buffer-templates nil)
(make-variable-buffer-local 'company-template--buffer-templates)

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

(defun company-template-add-field (templ pos text)
  (assert templ)
  (save-excursion
    (save-excursion
      (goto-char pos)
      (insert text)
      (when (> (point) (overlay-end templ))
        (move-overlay templ (overlay-start templ) (point))))
    (let ((ov (make-overlay pos (+ pos (length text))))
          (siblings (overlay-get templ 'company-template-fields)))
      ;; (overlay-put ov 'evaporate t)
      (overlay-put ov 'intangible t)
      (overlay-put ov 'face 'company-template-field)
      (overlay-put ov 'company-template-parent templ)
      (overlay-put ov 'insert-in-front-hooks '(company-template-insert-hook))
      (push ov siblings)
      (overlay-put templ 'company-template-fields siblings))))

(defun company-template-remove-field (ovl &optional clear)
  (when (overlayp ovl)
    (when (overlay-buffer ovl)
      (when clear
        (delete-region (overlay-start ovl) (overlay-end ovl)))
      (delete-overlay ovl))
    (let* ((templ (overlay-get ovl 'company-template-parent))
           (siblings (overlay-get templ 'company-template-fields)))
      (setq siblings (delq ovl siblings))
      (overlay-put templ 'company-template-fields siblings))))

(defun company-template-clean-up (&optional pos)
  "Clean up all templates that don't contain POS."
  (unless pos (setq pos (point)))
  (let ((local-ovs (overlays-in (- pos 2) pos)))
    (dolist (templ company-template--buffer-templates)
      (unless (and (memq templ local-ovs)
                   (overlay-get templ 'company-template-fields))
        (company-template-remove-template templ)))))

;; hooks ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun company-template-insert-hook (ovl after-p &rest ignore)
  "Called when a snippet input prompt is modified."
  (unless after-p
    (company-template-remove-field ovl t)))

(defun company-template-post-command ()
  (company-template-clean-up)
  (unless company-template--buffer-templates
    (remove-hook 'post-command-hook 'company-template-post-command t)))

(provide 'company-template)
;;; company-template.el ends here
