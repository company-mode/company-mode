;;; company-childframe.el --- Graphical popup frontend for Company  -*- lexical-binding: t -*-

;; Copyright (C) 2026  Free Software Foundation, Inc.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:
;;
;; Tooltip completion menu frontend for Company that uses a child frame.
;;
;; A lot of the code here was imported from the package `company-posframe',
;; credit to Clément Pit-Claudel, Feng Shu and others.

;;; Code:

(require 'company)
(require 'posframe)

(defgroup company-childframe nil
  "Group group group"
  :group 'company)

(defcustom company-childframe-font nil
  "The font used by company-childframe's frame.
Using current frame's font if it is nil."
  :type 'face)

(defvar company-childframe-buffer " *company-childframe-buffer*"
  "company-childframe's buffer which used by posframe.")

(defvar company-childframe--frame nil)

(defvar company-childframe-show-params nil
  "List of extra parameters passed to `posframe-show' in
  `company-childframe-show'.")

(defvar company-childframe-last-status nil)

(defvar company-childframe-buffer-map
  (let ((keymap (make-sparse-keymap)))
    (set-keymap-parent keymap company-active-map)
    ;; FIXME: Implement mouse scrolling commands.
    (define-key keymap [wheel-down] 'ignore)
    (define-key keymap [wheel-up] 'ignore)
    keymap)
  "Keymap for the child frame's popup/buffer.")

(defvar company-childframe-poshandler
  #'company-childframe-show-at-prefix
  "Poshandler for the completion dialog.")

(defun company-childframe-show-at-prefix (info)
  "Poshandler showing `company-childframe' at `company-prefix'."
  (let* ((parent-window (plist-get info :parent-window))
         (point (with-current-buffer (window-buffer parent-window)
                  (- (plist-get info :position)
                     (plist-get info :company-prefix-length))))
         (posn (posn-at-point point parent-window))
         ;; TODO: Strictly speaking, if company-childframe-font is not nil, that
         ;; should be used to find the default width...
         (expected-margin-width (* (plist-get info :company-margin) (default-font-width)))
         (xy (posn-x-y posn)))
    (setcar xy (- (car xy) expected-margin-width))
    (posframe-poshandler-point-bottom-left-corner (plist-put info :position posn))))

(defun company-childframe-show ()
  "Show company-childframe candidate menu."
  (let* ((x-wait-for-event-timeout nil)
         ;; Above: real effect (less flicker), below: just seem sensible.
         (inhibit-redisplay t)
         (before-make-frame-hook)
         (after-make-frame-functions)
         (x-fast-protocol-requests t)
         (height (min company-tooltip-limit company-candidates-length))
         (company-lines (company--create-lines company-selection height))
         (margin (car company-lines))
         (lines (cdr company-lines))
         (width (max (min (length (car lines))
                          company-tooltip-maximum-width)
                     company-tooltip-minimum-width))
         (contents (mapconcat #'identity lines "\n"))
         (buffer (get-buffer-create company-childframe-buffer)))
    (with-current-buffer buffer
      (use-local-map company-childframe-buffer-map))
    (apply #'posframe-show buffer
           :string contents
           :min-height height
           :min-width (+ company-tooltip-minimum-width
                         (* 2 company-tooltip-margin))
           :max-width (+ company-tooltip-maximum-width
                         (* 2 company-tooltip-margin))
           :font company-childframe-font
           :background-color (face-attribute 'company-tooltip :background)
           :lines-truncate t
           :poshandler company-childframe-poshandler
           :poshandler-extra-info
           (list :company-margin margin
                 :company-prefix-length (length company-prefix))
           company-childframe-show-params)))

(defun company-childframe-hide ()
  "Hide company-childframe candidate menu."
  (posframe-hide company-childframe-buffer))

(defun company-childframe-frontend (command)
  "`company-mode' frontend using childframe.
For COMMAND refer to `company-frontends'."
  (setq company-childframe-last-status
        (list (selected-window)
              (current-buffer)))
  (cl-case command
    (pre-command
     (when (not (posframe-workable-p))
       (user-error "Child frames not supported")))
    (hide
     (company-childframe-hide))
    (post-command
     (company-childframe-show))
    (select-mouse
     (company-childframe--select-mouse))))

(defun company-childframe--select-mouse ()
  (let ((event-col-row (company--event-col-row company-mouse-event))
        (event-window (posn-window (event-start company-mouse-event)))
        (parent-frame (frame-parameter nil 'parent-frame))
        (parent-buffer (frame-parameter nil 'posframe-parent-buffer)))
    (cond ((and event-window
                parent-frame
                parent-buffer
                (equal (buffer-name (window-buffer event-window))
                       company-childframe-buffer))
           (select-frame parent-frame)
           (select-window (get-buffer-window (cdr parent-buffer)))
           (company-set-selection (+ (cdr event-col-row)
                                     company-tooltip-offset
                                     (if (and (eq company-tooltip-offset-display 'lines)
                                              (not (zerop company-tooltip-offset)))
                                         -1 0)))
           t))))

(defun company-childframe-unless-just-one-frontend (command)
  "`company-childframe-frontend', but not shown for single candidates."
  (unless (and (memq command '(post-command unhide))
               (company--show-inline-p))
    (company-childframe-frontend command)))

(defun company-childframe-window-change ()
  "Hide posframe on window change."
  (when (posframe-workable-p)
    (unless (or (equal (buffer-name) company-childframe-buffer)
                (equal company-childframe-last-status
                       (list (selected-window)
                             (current-buffer))))
      (company-childframe-hide))))

(add-hook 'window-configuration-change-hook
          #'company-childframe-window-change)

(provide 'company-childframe)
;;; company-childframe.el ends here
