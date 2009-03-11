(eval-when-compile (require 'cl))

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

(defcustom company-backends '(company-elisp-completion)
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

(defvar company-mode-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "M-n") 'company-select-next)
    (define-key keymap (kbd "M-p") 'company-select-previous)
    (define-key keymap (kbd "M-<return>") 'company-complete-selection)
    (define-key keymap "\t" 'company-complete-common)
    keymap))

;;;###autoload
(define-minor-mode company-mode
  ""
  nil " comp" company-mode-map
  (if company-mode
      (progn
        (add-hook 'pre-command-hook 'company-pre-command nil t)
        (add-hook 'post-command-hook 'company-post-command nil t)
        (company-timer-set 'company-idle-delay
                           company-idle-delay))
    (remove-hook 'pre-command-hook 'company-pre-command t)
    (remove-hook 'post-command-hook 'company-post-command t)
    (company-cancel)))

;;; backends ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun company-grab (regexp &optional expression)
  (when (looking-back regexp)
    (or (match-string-no-properties (or expression 0)) "")))

(defun company-in-string-or-comment (&optional point)
  (let ((pos (syntax-ppss)))
    (or (nth 3 pos) (nth 4 pos) (nth 7 pos))))

;;; elisp

(defvar company-lisp-symbol-regexp
  "\\_<\\(\\sw\\|\\s_\\)+\\_>\\=")

(defun company-grab-lisp-symbol ()
  (let ((prefix (or (company-grab company-lisp-symbol-regexp) "")))
    (unless (and (company-in-string-or-comment (- (point) (length prefix)))
                 (/= (char-before (- (point) (length prefix))) ?`))
      prefix)))

(defun company-elisp-completion (command &optional arg &rest ignored)
  (case command
    ('prefix (and (eq major-mode 'emacs-lisp-mode)
                  (company-grab-lisp-symbol)))
    ('candidates (let ((completion-ignore-case nil))
                   (all-completions arg obarray
                                    (lambda (symbol) (or (boundp symbol)
                                                         (fboundp symbol))))))))

;;; completion mechanism ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar company-backend nil)
(make-variable-buffer-local 'company-backend)

(defvar company-prefix nil)
(make-variable-buffer-local 'company-prefix)

(defvar company-candidates nil)
(make-variable-buffer-local 'company-candidates)

(defvar company-common nil)
(make-variable-buffer-local 'company-common)

(defvar company-selection 0)
(make-variable-buffer-local 'company-selection)

(defvar company-selection-changed nil)
(make-variable-buffer-local 'company-selection-changed)

(defvar company-point nil)
(make-variable-buffer-local 'company-point)

(defsubst company-strip-prefix (str)
  (substring str (length company-prefix)))

(defsubst company-offset (display-limit)
  (let ((offset (- company-selection display-limit -1)))
    (max offset 0)))

(defsubst company-should-complete (prefix)
  (and (eq company-idle-delay t)
       (>= (length prefix) company-minimum-prefix-length)))

(defun company-idle-begin ()
  (and company-mode
       (not company-candidates)
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

(defun company-continue-or-cancel ()
  (when company-candidates
    (let ((old-point (- company-point (length company-prefix)))
          (company-idle-delay t)
          (company-minimum-prefix-length 0))
      ;; TODO: Make more efficient.
      (setq company-candidates nil)
      (company-begin)
      (unless (and company-candidates
                   (equal old-point (- company-point (length company-prefix))))
        (company-cancel))
      company-candidates)))

(defun company-begin ()
  (or (company-continue-or-cancel)
      (let ((completion-ignore-case nil) ;; TODO: make this optional
            prefix)
        (dolist (backend company-backends)
          (when (setq prefix (funcall backend 'prefix))
            (when (company-should-complete prefix)
              (setq company-backend backend
                    company-prefix prefix
                    company-candidates
                    (funcall company-backend 'candidates prefix)
                    company-common (try-completion prefix company-candidates)
                    company-selection 0
                    company-point (point)))
            (return prefix)))
        (unless (and company-candidates
                     (not (eq t company-common)))
          (company-cancel)))))

(defun company-cancel ()
  (setq company-backend nil
        company-prefix nil
        company-candidates nil
        company-common nil
        company-selection 0
        company-selection-changed nil
        company-point nil)
  (company-pseudo-tooltip-hide))

(defun company-pre-command ()
  (company-preview-hide)
  (company-pseudo-tooltip-hide))

(defun company-post-command ()
  (unless (equal (point) company-point)
    (company-begin))
  (when company-candidates
    (company-pseudo-tooltip-show-at-point (- (point) (length company-prefix))
                                          company-candidates
                                          company-selection)
    (company-preview-show-at-point (point) company-candidates
                                   company-selection)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun company-select-next ()
  (interactive)
  (when (company-manual-begin)
    (setq company-selection (min (1- (length company-candidates))
                                 (1+ company-selection))
          company-selection-changed t)))

(defun company-select-previous ()
  (interactive)
  (when (company-manual-begin)
    (setq company-selection (max 0 (1- company-selection))
          company-selection-changed t)))

(defun company-complete-selection ()
  (interactive)
  (when (company-manual-begin)
    (insert (company-strip-prefix (nth company-selection company-candidates)))))

(defun company-complete-common ()
  (interactive)
  (when (company-manual-begin)
    (insert (company-strip-prefix company-common))))

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

;;; pseudo-tooltip ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar company-pseudo-tooltip-overlay nil)
(make-variable-buffer-local 'company-pseudo-tooltip-overlay)

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

(defun company-fill-propertize-lines (column lines selection)
  (let ((width 0)
        (lines-copy lines)
        (len (min company-tooltip-limit (length lines)))
        new)
    (dotimes (i len)
      (setq width (max (length (pop lines-copy)) width)))
    (setq width (min width (- (window-width) column)))
    (dotimes (i len)
      (push (company-fill-propertize (pop lines) width (equal i selection))
            new))
    (nreverse new)))

;;; replace

(defun company-buffer-lines (beg end)
  (goto-char beg)
  (let ((row (cdr (posn-col-row (posn-at-point))))
        lines)
    (while (< (point) end)
      (move-to-window-line (incf row))
      (push (buffer-substring beg (min end (1- (point)))) lines)
      (setq beg (point)))
    (nreverse lines)))

(defun company-modify-line (old new offset)
  (concat (company-safe-substring old 0 offset)
          new
          (company-safe-substring old (+ offset (length new)))))

(defun company-modified-substring (beg end lines column)
  (let ((old (company-buffer-lines beg end))
        new)
    ;; Inject into old lines.
    (while old
      (push (company-modify-line (pop old) (pop lines) column) new))
    ;; Append whole new lines.
    (while lines
      (push (company-modify-line "" (pop lines) column) new))
    (concat (mapconcat 'identity (nreverse new) "\n")
            "\n")))

;; show

(defun company-pseudo-tooltip-show (row column lines &optional selection)
  (company-pseudo-tooltip-hide)
  (unless lines (error "No text provided"))
  (save-excursion

    ;; Scroll to offset.
    (let ((offset (company-offset company-tooltip-limit)))
      (setq lines (nthcdr offset lines))
      (decf selection offset))

    (setq lines (company-fill-propertize-lines column lines selection))


    (move-to-column 0)
    (move-to-window-line row)
    (let ((beg (point))
          (end (save-excursion
                 (move-to-window-line (min (window-height)
                                           (+ row company-tooltip-limit)))
                 (point)))
          str)

      (setq company-pseudo-tooltip-overlay (make-overlay beg end))

      (overlay-put company-pseudo-tooltip-overlay 'before-string
                   (company-modified-substring beg end lines column))
      (overlay-put company-pseudo-tooltip-overlay 'invisible t)
      (overlay-put company-pseudo-tooltip-overlay 'window (selected-window)))))

(defun company-pseudo-tooltip-show-at-point (pos text &optional selection)
  (let ((col-row (posn-col-row (posn-at-point pos))))
    (company-pseudo-tooltip-show (1+ (cdr col-row))
                                 (car col-row) text selection)))

(defun company-pseudo-tooltip-hide ()
  (when company-pseudo-tooltip-overlay
    (delete-overlay company-pseudo-tooltip-overlay)
    (setq company-pseudo-tooltip-overlay nil)))

;;; overlay ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar company-preview-overlay nil)
(make-variable-buffer-local 'company-preview-overlay)

(defun company-preview-show-at-point (pos text &optional selection)
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

(provide 'company)
;;; company.el ends here
