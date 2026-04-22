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

(defcustom company-childframe-border-width 1
  "The width of the popup's border, in graphical frames.

Users of HiDPI screens might like to set it to 2."
  :type 'integer)

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
    (define-key keymap [wheel-down] 'company-childframe-wheel-up)
    (define-key keymap [wheel-up] 'company-childframe-wheel-down)
    keymap)
  "Keymap for the child frame's popup/buffer.")

(defun company-childframe-wheel-up ()
  "Scroll up the displayed candidates."
  (interactive)
  (company-childframe--wheel-scroll 3))

(defun company-childframe-wheel-down ()
  "Scroll up the displayed candidates."
  (interactive)
  (company-childframe--wheel-scroll -3))

(defun company-childframe--wheel-scroll (amount)
  (let ((parent-frame (frame-parameter nil 'parent-frame))
        (parent-buffer (frame-parameter nil 'posframe-parent-buffer)))
    (when (and parent-frame
               parent-buffer)
      (select-frame parent-frame)
      (select-window (get-buffer-window (cdr parent-buffer)))
      (company-select-next amount))))

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
    (setcar xy (- (car xy) expected-margin-width (if (display-graphic-p)
                                                     company-childframe-border-width
                                                   0)))
    (posframe-poshandler-point-bottom-left-corner (plist-put info :position posn))))

(defun company-childframe-show ()
  "Show company-childframe candidate menu."
  (defvar x-wait-for-event-timeout)
  (defvar x-fast-protocol-requests)
  (let* (;; Should be unnecessary in Emacs 31+, debbugs#80662.
         (x-wait-for-event-timeout nil)
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
    (when (and (eq (frame-live-p company-childframe--frame) 'x)
               (not (eq (car (frame-list)) company-childframe--frame)))
      ;; Make sure it's the first in the list, to avoid premature sync when some
      ;; other frame is redisplayed first.  Again, non-atomic updated on X11.
      ;; https://debbugs.gnu.org/80662#185
      (delete-frame company-childframe--frame))
    (apply #'posframe-show buffer
           :string contents
           :height height
           :width (if (or (<= company-candidates-length
                              height)
                          (not (display-graphic-p)))
                      width
                    (1- width))
           :font company-childframe-font
           :background-color (face-attribute 'company-tooltip :background)
           :lines-truncate t
           :override-parameters '((inhibit-double-buffering . t))
           :border-width (and (display-graphic-p) company-childframe-border-width)
           ;; :border-color "light salmon"
           ;; :border-color "light steel blue"
           ;; We'll probably want a separate face for it.
           :border-color (face-attribute 'company-tooltip-scrollbar-track :background)
           :poshandler company-childframe-poshandler
           :poshandler-extra-info
           (list :company-margin margin
                 :company-prefix-length (length company-prefix))
           company-childframe-show-params)
    (with-current-buffer buffer
      (use-local-map company-childframe-buffer-map)
      (setq company-childframe--frame posframe--frame)
      ;; FIXME: Does not honor remappings by minor modes in the parent buffer,
      ;; e.g. the special behavior of C-d with parent-mode, etc.
      (add-hook 'pre-command-hook
                #'company-childframe--pre-command
                nil t))))

(defun company-childframe-hide ()
  "Hide company-childframe candidate menu."
  (when (and (frame-live-p company-childframe--frame)
             (frame-visible-p company-childframe--frame))
    ;; PGTK/NS/W32 protocols can update the display atomically.
    (when (eq window-system 'x)
      ;; Seems to help avoid the final flicker - probably by keeping the parent's
      ;; display matrix up to date (so it can repaint on Expose immediately).
      (redisplay))
    (make-frame-invisible company-childframe--frame)))

;;;###autoload
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
    (show (setq company--tooltip-current-width 0))
    (hide
     (company-childframe-hide))
    (post-command
     (company-childframe-show))
    (select-mouse
     (company-childframe--select-mouse))))

(defun company-childframe--select-mouse ()
  (let ((event-col-row (company--event-col-row company-mouse-event))
        (event-window (posn-window (event-start company-mouse-event))))
    (cond ((and event-window
                (equal (buffer-name (window-buffer event-window))
                       company-childframe-buffer))
           (company-set-selection (+ (cdr event-col-row)
                                     company-tooltip-offset
                                     (if (and (eq company-tooltip-offset-display 'lines)
                                              (not (zerop company-tooltip-offset)))
                                         -1 0)))
           t))))

(defun company-childframe--pre-command ()
  (let ((parent-frame (frame-parameter nil 'parent-frame))
        (parent-buffer (cdr (frame-parameter nil 'posframe-parent-buffer))))
    (when (and
           (not (memq this-command
                      '(company-childframe-wheel-up
                        company-childframe-wheel-down)))
           parent-frame parent-buffer)
      (select-frame parent-frame)
      (select-window (get-buffer-window parent-buffer)))))

;;;###autoload
(defun company-childframe-unless-just-one-frontend (command)
  "`company-childframe-frontend', but not shown for single candidates."
  (if (company--show-inline-p)
      (and (member command '(post-command hide))
           (company-childframe-hide))
    (and (memq command '(post-command show unhide hide select-mouse))
         (company-childframe-frontend command))))

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
