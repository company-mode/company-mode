;;; company.el --- extensible inline text completion mechanism
;;
;; Copyright (C) 2009 Nikolaj Schumacher
;;
;; Author: Nikolaj Schumacher <bugs * nschum de>
;; Version: 
;; Keywords: abbrev, convenience, matchis
;; URL: http://nschum.de/src/emacs/company/
;; Compatibility: GNU Emacs 23.x
;;
;; This file is NOT part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;;; Change Log:
;;
;;    Initial release.
;;
;;; Code:

(eval-when-compile (require 'cl))

(add-to-list 'debug-ignored-errors
             "^Pseudo tooltip frontend cannot be used twice$")
(add-to-list 'debug-ignored-errors "^Preview frontend cannot be used twice$")
(add-to-list 'debug-ignored-errors "^Echo area cannot be used twice$")
(add-to-list 'debug-ignored-errors "^No documentation available$")

(defgroup company nil
  ""
  :group 'abbrev
  :group 'convenience
  :group 'maching)

(defface company-tooltip
  '((t :background "yellow"
       :foreground "black"))
  "*"
  :group 'company)

(defface company-tooltip-selection
  '((t :background "orange1"
       :foreground "black"))
  "*"
  :group 'company)

(defface company-tooltip-common
  '((t :inherit company-tooltip
       :foreground "red"))
  "*"
  :group 'company)

(defface company-tooltip-common-selection
  '((t :inherit company-tooltip-selection
       :foreground "red"))
  "*"
  :group 'company)

(defcustom company-tooltip-limit 10
  "*"
  :group 'company
  :type 'integer)

(defface company-preview
  '((t :background "blue4"
       :foreground "wheat"))
  "*"
  :group 'company)

(defface company-preview-common
  '((t :inherit company-preview
       :foreground "red"))
  "*"
  :group 'company)

(defface company-echo nil
  "*"
  :group 'company)

(defface company-echo-common
  '((((background dark)) (:foreground "firebrick1"))
    (((background light)) (:background "firebrick4")))
  "*"
  :group 'company)

(defun company-frontends-set (variable value)
  ;; uniquify
  (let ((remainder value))
    (setcdr remainder (delq (car remainder) (cdr remainder))))
  (and (memq 'company-pseudo-tooltip-unless-just-one-frontend value)
       (memq 'company-pseudo-tooltip-frontend value)
       (error "Pseudo tooltip frontend cannot be used twice"))
  (and (memq 'company-preview-if-just-one-frontend value)
       (memq 'company-preview-frontend value)
       (error "Preview frontend cannot be used twice"))
  (and (memq 'company-echo value)
       (memq 'company-echo-metadata-frontend value)
       (error "Echo area cannot be used twice"))
  ;; preview must come last
  (dolist (f '(company-preview-if-just-one-frontend company-preview-frontend))
    (when (memq f value)
      (setq value (append (delq f value) (list f)))))
  (set variable value))

(defcustom company-frontends '(company-pseudo-tooltip-unless-just-one-frontend
                               company-preview-if-just-one-frontend
                               company-echo-metadata-frontend)
  "*"
  :set 'company-frontends-set
  :group 'company
  :type '(repeat (choice (const :tag "echo" company-echo-frontend)
                         (const :tag "pseudo tooltip"
                                company-pseudo-tooltip-frontend)
                         (const :tag "pseudo tooltip, multiple only"
                                company-pseudo-tooltip-unless-just-one-frontend)
                         (const :tag "preview" company-preview-frontend)
                         (const :tag "preview, unique only"
                                company-preview-if-just-one-frontend)
                         (function :tag "custom function" nil))))

(defcustom company-backends '(company-elisp company-nxml company-css
                              company-semantic company-oddmuse
                              company-dabbrev)
  "*"
  :group 'company
  :type '(repeat (function :tag "function" nil)))

(defcustom company-minimum-prefix-length 3
  "*"
  :group 'company
  :type '(integer :tag "prefix length"))

(defvar company-timer nil)

(defun company-timer-set (variable value)
  (set variable value)
  (when company-timer (cancel-timer company-timer))
  (when (numberp value)
    (setq company-timer (run-with-idle-timer value t 'company-idle-begin))))

(defcustom company-idle-delay .7
  "*"
  :set 'company-timer-set
  :group 'company
  :type '(choice (const :tag "never (nil)" nil)
                 (const :tag "immediate (t)" t)
                 (number :tag "seconds")))

;;; mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar company-current-map (make-sparse-keymap))

(defvar company-mode-map (make-sparse-keymap))

(defvar company-active-map
  (let ((keymap (make-sparse-keymap)))
    (set-keymap-parent keymap company-mode-map)
    (define-key keymap (kbd "M-n") 'company-select-next)
    (define-key keymap (kbd "M-p") 'company-select-previous)
    (define-key keymap "\C-m" 'company-complete-selection)
    (define-key keymap "\t" 'company-complete-common)
    (define-key keymap (kbd "<f1>") 'company-show-doc-buffer)
    keymap))

;;;###autoload
(define-minor-mode company-mode
  ""
  nil " comp" nil
  (if company-mode
      (progn
        (add-to-list 'minor-mode-overriding-map-alist
                     (cons 'company-mode company-current-map))
        (add-hook 'pre-command-hook 'company-pre-command nil t)
        (add-hook 'post-command-hook 'company-post-command nil t)
        (company-timer-set 'company-idle-delay
                           company-idle-delay))
    (remove-hook 'pre-command-hook 'company-pre-command t)
    (remove-hook 'post-command-hook 'company-post-command t)
    (company-cancel)
    (kill-local-variable 'company-point)))

;;; backends ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun company-grab (regexp &optional expression)
  (when (looking-back regexp)
    (or (match-string-no-properties (or expression 0)) "")))

(defun company-in-string-or-comment (&optional point)
  (let ((pos (syntax-ppss)))
    (or (nth 3 pos) (nth 4 pos) (nth 7 pos))))

;;; completion mechanism ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar company-backend nil)
(make-variable-buffer-local 'company-backend)

(defvar company-prefix nil)
(make-variable-buffer-local 'company-prefix)

(defvar company-candidates nil)
(make-variable-buffer-local 'company-candidates)

(defvar company-candidates-cache nil)
(make-variable-buffer-local 'company-candidates-cache)

(defvar company-common nil)
(make-variable-buffer-local 'company-common)

(defvar company-selection 0)
(make-variable-buffer-local 'company-selection)

(defvar company-selection-changed nil)
(make-variable-buffer-local 'company-selection-changed)

(defvar company-point nil)
(make-variable-buffer-local 'company-point)

(defvar company-disabled-backends nil)

(defsubst company-strip-prefix (str)
  (substring str (length company-prefix)))

(defsubst company-reformat (candidate)
  ;; company-ispell needs this, because the results are always lower-case
  ;; It's mory efficient to fix it only when they are displayed.
  (concat company-prefix (substring candidate (length company-prefix))))

(defsubst company-should-complete (prefix)
  (and (eq company-idle-delay t)
       (>= (length prefix) company-minimum-prefix-length)))

(defsubst company-call-frontends (command)
  (dolist (frontend company-frontends)
    (funcall frontend command)))

(defsubst company-calculate-candidates (prefix)
  (or (setq company-candidates (cdr (assoc prefix company-candidates-cache)))
      (let ((len (length prefix))
            (completion-ignore-case (funcall company-backend 'ignore-case))
            prev)
        (dotimes (i len)
          (when (setq prev (cdr (assoc (substring prefix 0 (- len i))
                                       company-candidates-cache)))
            (setq company-candidates (all-completions prefix prev))
            (return t))))
      (progn
        (setq company-candidates (funcall company-backend 'candidates prefix))
        (unless (funcall company-backend 'sorted)
          (setq company-candidates (sort company-candidates 'string<)))))
  (unless (assoc prefix company-candidates-cache)
    (push (cons prefix company-candidates) company-candidates-cache))
  (setq company-selection 0
        company-prefix prefix)
  (let ((completion-ignore-case (funcall company-backend 'ignore-case)))
    (setq company-common (try-completion company-prefix company-candidates)))
  (when (eq company-common t)
    (setq company-candidates nil))
  company-candidates)

(defun company-idle-begin ()
  (and company-mode
       (not company-candidates)
       (not (equal (point) company-point))
       (let ((company-idle-delay t))
         (company-begin)
         (company-post-command))))

(defun company-manual-begin ()
  (and company-mode
       (not company-candidates)
       (let ((company-idle-delay t)
             (company-minimum-prefix-length 0))
         (company-begin)))
  ;; Return non-nil if active.
  company-candidates)

(defun company-continue ()
  (when company-candidates
    (let ((new-prefix (funcall company-backend 'prefix)))
      (unless (and (= (- (point) (length new-prefix))
                      (- company-point (length company-prefix)))
                   (or (equal company-prefix new-prefix)
                       (company-calculate-candidates new-prefix)))
        (setq company-candidates nil)))))

(defun company-begin ()
  (company-continue)
  (unless company-candidates
    (let (prefix)
      (dolist (backend company-backends)
        (unless (fboundp backend)
          (ignore-errors (require backend nil t)))
        (if (fboundp backend)
            (when (setq prefix (funcall backend 'prefix))
              (when (company-should-complete prefix)
                (setq company-backend backend)
                (company-calculate-candidates prefix))
              (return prefix))
          (unless (memq backend company-disabled-backends)
            (push backend company-disabled-backends)
            (message "Company back-end '%s' could not be initialized"
                     backend))))))
  (if company-candidates
      (progn
        (setq company-point (point))
        (set-keymap-parent company-current-map company-active-map)
        (company-call-frontends 'update))
    (company-cancel)))

(defun company-cancel ()
  (setq company-backend nil
        company-prefix nil
        company-candidates nil
        company-candidates-cache nil
        company-common nil
        company-selection 0
        company-selection-changed nil
        company-point nil)
  (company-call-frontends 'hide)
  (set-keymap-parent company-current-map company-mode-map))

(defun company-abort ()
  (company-cancel)
  ;; Don't start again, unless started manually.
  (setq company-point (point)))

(defun company-pre-command ()
  (unless (eq this-command 'company-show-doc-buffer)
    (condition-case err
        (when company-candidates
          (company-call-frontends 'pre-command))
      (error (message "Company: An error occurred in pre-command")
             (message "%s" (error-message-string err))
             (company-cancel)))))

(defun company-post-command ()
  (unless (eq this-command 'company-show-doc-buffer)
    (condition-case err
        (progn
          (unless (equal (point) company-point)
            (company-begin))
          (when company-candidates
            (company-call-frontends 'post-command)))
      (error (message "Company: An error occurred in post-command")
             (message "%s" (error-message-string err))
             (company-cancel)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun company-select-next ()
  (interactive)
  (when (company-manual-begin)
    (setq company-selection (min (1- (length company-candidates))
                                 (1+ company-selection))
          company-selection-changed t))
  (company-call-frontends 'update))

(defun company-select-previous ()
  (interactive)
  (when (company-manual-begin)
    (setq company-selection (max 0 (1- company-selection))
          company-selection-changed t))
  (company-call-frontends 'update))

(defun company-complete-selection ()
  (interactive)
  (when (company-manual-begin)
    (insert (company-strip-prefix (nth company-selection company-candidates)))
    (company-abort)))

(defun company-complete-common ()
  (interactive)
  (when (company-manual-begin)
    (insert (company-strip-prefix company-common))))

(defun company-complete ()
  (interactive)
  (when (company-manual-begin)
    (if (or company-selection-changed
            (eq last-command 'company-complete-common))
        (call-interactively 'company-complete-selection)
      (call-interactively 'company-complete-common)
      (setq this-command 'company-complete-common))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst company-space-strings-limit 100)

(defconst company-space-strings
  (let (lst)
    (dotimes (i company-space-strings-limit)
      (push (make-string (- company-space-strings-limit 1 i) ?\  ) lst))
    (apply 'vector lst)))

(defsubst company-space-string (len)
  (if (< len company-space-strings-limit)
      (aref company-space-strings len)
    (make-string len ?\ )))

(defsubst company-safe-substring (str from &optional to)
  (let ((len (length str)))
    (if (> from len)
        ""
      (if (and to (> to len))
          (concat (substring str from)
                  (company-space-string (- to len)))
        (substring str from to)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar company-last-metadata nil)
(make-variable-buffer-local 'company-last-metadata)

(defun company-fetch-metadata ()
  (let ((selected (nth company-selection company-candidates)))
    (unless (equal selected (car company-last-metadata))
      (setq company-last-metadata
            (cons selected (funcall company-backend 'meta selected))))
    (cdr company-last-metadata)))

(defun company-doc-buffer (&optional string)
  (with-current-buffer (get-buffer-create "*Company meta-data*")
    (erase-buffer)
    (current-buffer)))

(defun company-show-doc-buffer ()
  (interactive)
  (when company-candidates
    (save-window-excursion
      (let* ((selected (nth company-selection company-candidates))
             (buffer (funcall company-backend 'doc-buffer selected)))
        (if (not buffer)
            (error "No documentation available.")
          (display-buffer buffer)
          (read-event)
          (when last-input-event
            (clear-this-command-keys t)
            (setq unread-command-events (list last-input-event))))))))

;;; pseudo-tooltip ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar company-pseudo-tooltip-overlay nil)
(make-variable-buffer-local 'company-pseudo-tooltip-overlay)

(defvar company-tooltip-offset 0)
(make-variable-buffer-local 'company-tooltip-offset)

(defun company-pseudo-tooltip-update-offset (selection num-lines limit)

  (decf limit 2)
  (setq company-tooltip-offset
        (max (min selection company-tooltip-offset)
             (- selection -1 limit)))

  (when (<= company-tooltip-offset 1)
    (incf limit)
    (setq company-tooltip-offset 0))

  (when (>= company-tooltip-offset (- num-lines limit 1))
    (incf limit)
    (when (= selection (1- num-lines))
      (setq company-tooltip-offset (max (1- company-tooltip-offset) 0))))

  limit)

;;; propertize

(defun company-fill-propertize (line width selected)
  (setq line (company-safe-substring line 0 width))
  (add-text-properties 0 width
                       (list 'face (if selected
                                       'company-tooltip-selection
                                     'company-tooltip)) line)
  (add-text-properties 0 (length company-common)
                       (list 'face (if selected
                                       'company-tooltip-common-selection
                                     'company-tooltip-common)) line)
  line)

;;; replace

(defun company-buffer-lines (beg end)
  (goto-char beg)
  (let ((row (cdr (posn-col-row (posn-at-point))))
        lines)
    (while (and (equal (move-to-window-line (incf row)) row)
                (<= (point) end))
      (push (buffer-substring beg (min end (1- (point)))) lines)
      (setq beg (point)))
    (unless (eq beg end)
      (push (buffer-substring beg end) lines))
    (nreverse lines)))

(defun company-modify-line (old new offset)
  (concat (company-safe-substring old 0 offset)
          new
          (company-safe-substring old (+ offset (length new)))))

(defun company-replacement-string (old lines column nl)
  (let (new)
    ;; Inject into old lines.
    (while old
      (push (company-modify-line (pop old) (pop lines) column) new))
    ;; Append whole new lines.
    (while lines
      (push (company-modify-line "" (pop lines) column) new))
    (concat (when nl "\n")
            (mapconcat 'identity (nreverse new) "\n")
            "\n")))

(defun company-create-lines (column lines selection)

  (let ((limit (max company-tooltip-limit 3))
        (len (length lines))
        width
        lines-copy
        previous
        remainder
        new)

    ;; Scroll to offset.
    (setq limit (company-pseudo-tooltip-update-offset selection len limit))

    (when (> company-tooltip-offset 0)
      (setq previous (format "...(%d)" company-tooltip-offset)))

    (setq remainder (- len limit company-tooltip-offset)
          remainder (when (> remainder 0)
                      (setq remainder (format "...(%d)" remainder))))

    (decf selection company-tooltip-offset)
    (setq width (min (length previous) (length remainder))
          lines (nthcdr company-tooltip-offset lines)
          len (min limit (length lines))
          lines-copy lines)

    (dotimes (i len)
      (setq width (max (length (pop lines-copy)) width)))
    (setq width (min width (- (window-width) column)))

    (when previous
      (push (propertize (company-safe-substring previous 0 width)
                        'face 'company-tooltip)
            new))

    (dotimes (i len)
      (push (company-fill-propertize (company-reformat (pop lines))
                                     width (equal i selection))
            new))

    (when remainder
      (push (propertize (company-safe-substring remainder 0 width)
                        'face 'company-tooltip)
            new))

    (setq lines (nreverse new))))

;; show

(defun company-pseudo-tooltip-show (row column lines selection)
  (company-pseudo-tooltip-hide)
  (unless lines (error "No text provided"))
  (save-excursion

    (move-to-column 0)

    (let* ((lines (company-create-lines column lines selection))
           (nl (< (move-to-window-line row) row))
           (beg (point))
           (end (save-excursion
                  (move-to-window-line (min (window-height)
                                            (+ row company-tooltip-limit)))
                  (point)))
           (old-string (company-buffer-lines beg end))
           str)

      (setq company-pseudo-tooltip-overlay (make-overlay beg end))

      (overlay-put company-pseudo-tooltip-overlay 'company-old old-string)
      (overlay-put company-pseudo-tooltip-overlay 'company-column column)
      (overlay-put company-pseudo-tooltip-overlay 'company-nl nl)
      (overlay-put company-pseudo-tooltip-overlay 'company-before
                   (company-replacement-string old-string lines column nl))

      (overlay-put company-pseudo-tooltip-overlay 'window (selected-window)))))

(defun company-pseudo-tooltip-show-at-point (pos)
  (let ((col-row (posn-col-row (posn-at-point pos))))
    (company-pseudo-tooltip-show (1+ (cdr col-row)) (car col-row)
                                 company-candidates company-selection)))

(defun company-pseudo-tooltip-edit (lines selection)
  (let* ((old-string (overlay-get company-pseudo-tooltip-overlay 'company-old))
         (column (overlay-get company-pseudo-tooltip-overlay 'company-column))
         (nl (overlay-get company-pseudo-tooltip-overlay 'company-nl))
         (lines (company-create-lines column lines selection)))
    (overlay-put company-pseudo-tooltip-overlay 'company-before
                 (company-replacement-string old-string lines column nl))))

(defun company-pseudo-tooltip-hide ()
  (when company-pseudo-tooltip-overlay
    (delete-overlay company-pseudo-tooltip-overlay)
    (setq company-pseudo-tooltip-overlay nil)))

(defun company-pseudo-tooltip-hide-temporarily ()
  (when (overlayp company-pseudo-tooltip-overlay)
    (overlay-put company-pseudo-tooltip-overlay 'invisible nil)
    (overlay-put company-pseudo-tooltip-overlay 'before-string nil)))

(defun company-pseudo-tooltip-unhide ()
  (when company-pseudo-tooltip-overlay
    (overlay-put company-pseudo-tooltip-overlay 'invisible t)
    (overlay-put company-pseudo-tooltip-overlay 'before-string
                 (overlay-get company-pseudo-tooltip-overlay 'company-before))))

(defun company-pseudo-tooltip-frontend (command)
  (case command
    ('pre-command (company-pseudo-tooltip-hide-temporarily))
    ('post-command
     (unless (overlayp company-pseudo-tooltip-overlay)
       (company-pseudo-tooltip-show-at-point (- (point)
                                                (length company-prefix))))
     (company-pseudo-tooltip-unhide))
    ('hide (company-pseudo-tooltip-hide)
           (setq company-tooltip-offset 0))
    ('update (when (overlayp company-pseudo-tooltip-overlay)
               (company-pseudo-tooltip-edit company-candidates
                                            company-selection)))))

(defun company-pseudo-tooltip-unless-just-one-frontend (command)
  (unless (and (eq command 'post-command)
               (not (cdr company-candidates)))
    (company-pseudo-tooltip-frontend command)))

;;; overlay ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar company-preview-overlay nil)
(make-variable-buffer-local 'company-preview-overlay)

(defun company-preview-show-at-point (pos)
  (company-preview-hide)

  (setq company-preview-overlay (make-overlay pos pos))

  (let ((completion (company-strip-prefix (nth company-selection
                                               company-candidates))))
    (and (equal pos (point))
         (not (equal completion ""))
         (add-text-properties 0 1 '(cursor t) completion))

    (setq completion (propertize completion 'face 'company-preview))
    (add-text-properties 0 (- (length company-common) (length company-prefix))
                         '(face company-preview-common) completion)

    (overlay-put company-preview-overlay 'after-string completion)
    (overlay-put company-preview-overlay 'window (selected-window))))

(defun company-preview-hide ()
  (when company-preview-overlay
    (delete-overlay company-preview-overlay)
    (setq company-preview-overlay nil)))

(defun company-preview-frontend (command)
  (case command
    ('pre-command (company-preview-hide))
    ('post-command (company-preview-show-at-point (point)))
    ('hide (company-preview-hide))))

(defun company-preview-if-just-one-frontend (command)
  (unless (and (eq command 'post-command)
               (cdr company-candidates))
    (company-preview-frontend command)))

;;; echo ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar company-echo-last-msg nil)
(make-variable-buffer-local 'company-echo-last-msg)

(defun company-echo-refresh ()
  (let ((message-log-max nil))
    (if company-echo-last-msg
        (message "%s" company-echo-last-msg)
      (message ""))))

(defun company-echo-show (candidates)

  ;; Roll to selection.
  (setq candidates (nthcdr company-selection candidates))

  (let ((limit (window-width (minibuffer-window)))
        (len -1)
        comp msg)
    (while candidates
      (setq comp (company-reformat (pop candidates))
            len (+ len 1 (length comp)))
      (if (>= len limit)
          (setq candidates nil)
        (setq comp (propertize comp 'face 'company-echo))
        (add-text-properties 0 (length company-common)
                             '(face company-echo-common) comp)
        (push comp msg)))

    (setq company-echo-last-msg (mapconcat 'identity (nreverse msg) " "))
    (company-echo-refresh)))

(defun company-echo-frontend (command)
  (case command
    ('pre-command (company-echo-refresh))
    ('post-command (company-echo-show company-candidates))
    ('hide (setq company-echo-last-msg nil))))

(defun company-echo-metadata-frontend (command)
  (case command
    ('pre-command (company-echo-refresh))
    ('post-command (setq company-echo-last-msg (company-fetch-metadata))
                   (company-echo-refresh))
    ('hide (setq company-echo-last-msg nil))))


(provide 'company)
;;; company.el ends here
