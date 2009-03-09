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

(defcustom company-tooltip-limit 10
  "*"
  :group 'company
  :type 'integer)

(defcustom company-backends '(company-elisp-completion)
  "*"
  :group 'company
  :type '(repeat (function :tag "function" nil)))

;;; mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar company-mode-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "M-n") 'company-select-next)
    (define-key keymap (kbd "M-p") 'company-select-previous)
    (define-key keymap (kbd "M-<return>") 'company-complete-selection)
    keymap))

;;;###autoload
(define-minor-mode company-mode
  ""
  nil " comp" company-mode-map
  (if company-mode
      (progn
        (add-hook 'pre-command-hook 'company-pre-command nil t)
        (add-hook 'post-command-hook 'company-post-command nil t))
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

(defvar company-selection 0)
(make-variable-buffer-local 'company-selection)

(defvar company-point nil)
(make-variable-buffer-local 'company-point)

(defsubst company-strip-prefix (str)
  (substring str (length company-prefix)))

(defun company-begin ()
  (let ((completion-ignore-case nil) ;; TODO: make this optional
        prefix)
    (dolist (backend company-backends)
      (when (setq prefix (funcall backend 'prefix))
        (setq company-backend backend
              company-prefix prefix
              company-candidates
              (funcall company-backend 'candidates prefix)
              company-selection 0
              company-point (point))
        (return prefix)))
    (unless (or (cdr company-candidates)
                (when company-candidates
                  (not (equal (car company-candidates) company-prefix))))
      (company-cancel))))

(defun company-cancel ()
  (setq company-backend nil
        company-prefix nil
        company-candidates nil
        company-selection 0
        company-point nil)
  (company-pseudo-tooltip-hide))

(defun company-pre-command ()
  (company-pseudo-tooltip-hide))

(defun company-post-command ()
  (unless (equal (point) company-point)
    (company-begin))
  (when company-candidates
    (company-pseudo-tooltip-show-at-point (- (point) (length company-prefix))
                                          company-candidates
                                          company-selection)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun company-select-next ()
  (interactive)
  (setq company-selection (min (1- (length company-candidates))
                               (1+ company-selection))))

(defun company-select-previous ()
  (interactive)
  (setq company-selection (max 0 (1- company-selection))))

(defun company-complete-selection ()
  (interactive)
  (insert (company-strip-prefix (nth company-selection company-candidates))))

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

(defun company-fill-propertize (line width face)
  (setq line (company-safe-substring line 0 width))
  (add-text-properties 0 width (list 'face face) line)
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
      (push (company-fill-propertize (pop lines) width
                                     (if (equal i selection)
                                         'company-tooltip-selection
                                       'company-tooltip))
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

(provide 'company)
;;; company.el ends here
