;;; company-tng.el --- company-mode frontend that inserts a candidate
;;; into the buffer as soon as it's selected, Vim style

;; Copyright (C) 2017  Free Software Foundation, Inc.

;; Author: Nikita Leshenko

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


;;; Commentary:
;;
;; company-tng (tab and go) allows you to use TAB to both select a
;; completion candidate from the list and to insert it into the
;; buffer.
;;
;; It cycles the candidates like `yank-pop' or `dabbrev-expand' or
;; Vim: Pressing TAB selects the first item in the completion menu and
;; inserts it in the buffer. Pressing TAB again selects the second
;; item and replaces the inserted item with the second one. This can
;; continue as long as the user wishes to cycle through the menu.
;;
;; The benefits are that there is only one shortcut key to interact
;; with and there is no need to confirm an entry.

;;; Code:

(require 'company)
(require 'cl-lib)

(defvar-local company-tng--overlay nil)

;;;###autoload
(defun company-tng-frontend (command)
  "When the user changes the selection at least once, this
frontend will display the candidate in the buffer as if it's
already there and any key outside of `company-active-map' will
confirm the selection and finish the completion."
  (cl-case command
    (show
     (let ((ov (make-overlay (point) (point))))
       (setq company-tng--overlay ov)
       (overlay-put ov 'priority 2))
     (advice-add 'company-select-next :before-until 'company-tng--allow-unselected))
    (update
     (let ((ov company-tng--overlay)
           (selected (nth company-selection company-candidates))
           (prefix (length company-prefix)))
       (move-overlay ov (- (point) prefix) (point))
       (overlay-put ov 'display (and company-selection-changed selected))))
    (hide
     (when company-tng--overlay
       (delete-overlay company-tng--overlay))
     (advice-remove 'company-select-next 'company-tng--allow-unselected))
    (pre-command
     (when (and company-selection-changed
                (not (company--company-command-p (this-command-keys))))
       (company--unread-this-command-keys)
       (setq this-command 'company-complete-selection)))))

(defun company-tng--allow-unselected (&optional arg)
  "Advice `company-select-next' to allow for an 'unselected'
state. Unselected means that no user interaction took place on the
completion candidates and it's marked by setting
`company-selection-changed' to nil. This advice will call the underlying
`company-select-next' unless we need to transition to or from an unselected
state.

Possible state transitions:
- (arg > 0) unselected -> first candidate selected
- (arg < 0) first candidate selected -> unselected
- (arg < 0 wrap-round) unselected -> last candidate selected
- (arg < 0 no wrap-round) unselected -> unselected

There is no need to advice `company-select-previous' because it calls
`company-select-next' internally."
  (cond
   ;; Selecting next
   ((or (not arg) (> arg 0))
    (unless company-selection-changed
      (company-set-selection (1- (or arg 1)) 'force-update)
      t))
   ;; Selecting previous
   ((< arg 0)
    (when (and company-selection-changed
               (< (+ company-selection arg) 0))
      (company-set-selection 0)
      (setq company-selection-changed nil)
      (company-call-frontends 'update)
      t)
    )))

(provide 'company-tng)
;;; company-tng.el ends here