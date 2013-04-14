;;; company-clang.el --- company-mode completion back-end for Clang

;; Copyright (C) 2009, 2011, 2013  Free Software Foundation, Inc.

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


;;; Commentary:
;;

;;; Code:

(require 'company)
(require 'company-template)
(eval-when-compile (require 'cl))

(defgroup company-clang nil
  "Completion back-end for Clang."
  :group 'company)

(defcustom company-clang-executable
  (executable-find "clang")
  "Location of clang executable."
  :type 'file)

(defcustom company-clang-auto-save t
  "Determines whether to save the buffer when retrieving completions.
clang can only complete correctly when the buffer has been saved."
  :type '(choice (const :tag "Off" nil)
                 (const :tag "On" t)))

(defcustom company-clang-arguments nil
  "Additional arguments to pass to clang when completing.
Prefix files (-include ...) can be selected with
`company-clang-set-prefix' or automatically through a custom
`company-clang-prefix-guesser'."
  :type '(repeat (string :tag "Argument" nil)))

(defcustom company-clang-prefix-guesser 'company-clang-guess-prefix
  "A function to determine the prefix file for the current buffer."
  :type '(function :tag "Guesser function" nil))

(defvar company-clang-modes '(c-mode c++-mode objc-mode)
  "Major modes which clang may complete.")

;; prefix ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar company-clang--prefix nil)

(defsubst company-clang--guess-pch-file (file)
  (let ((dir (directory-file-name (file-name-directory file))))
    (when (equal (file-name-nondirectory dir) "Classes")
      (setq dir (file-name-directory dir)))
    (car (directory-files dir t "\\([^.]h\\|[^h]\\).pch\\'" t))))

(defsubst company-clang--file-substring (file beg end)
  (with-temp-buffer
    (insert-file-contents-literally file nil beg end)
    (buffer-string)))

(defun company-clang-guess-prefix ()
  "Try to guess the prefix file for the current buffer."
  ;; Prefixes seem to be called .pch.  Pre-compiled headers do, too.
  ;; So we look at the magic number to rule them out.
  (let* ((file (company-clang--guess-pch-file buffer-file-name))
         (magic-number (and file (company-clang--file-substring file 0 4))))
    (unless (member magic-number '("CPCH" "gpch"))
      file)))

(defun company-clang-set-prefix (&optional prefix)
  "Use PREFIX as a prefix (-include ...) file for clang completion."
  (interactive (let ((def (funcall company-clang-prefix-guesser)))
     (unless (stringp def)
       (setq def default-directory))
     (list (read-file-name "Prefix file: "
                           (when def (file-name-directory def))
                           def t (when def (file-name-nondirectory def))))))
  ;; TODO: pre-compile?
  (setq company-clang--prefix (and (stringp prefix)
                                   (file-regular-p prefix)
                                   prefix)))

;; Clean-up on exit.
(add-hook 'kill-emacs-hook 'company-clang-set-prefix)

;; parsing ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: Handle Pattern (syntactic hints would be neat).
;; Do we ever see OVERLOAD (or OVERRIDE)?
(defconst company-clang--completion-pattern
  "^COMPLETION: \\_<\\(%s[a-zA-Z0-9_:]*\\)\\(?: : \\(.*\\)$\\)?$")

(defconst company-clang--error-buffer-name "*clang error*")

(defvar company-clang--meta-cache nil)

(defun company-clang--parse-output (prefix objc)
  (goto-char (point-min))
  (let ((pattern (format company-clang--completion-pattern
                         (regexp-quote prefix)))
        (case-fold-search nil)
        lines match)
    (setq company-clang--meta-cache (make-hash-table :test 'equal))
    (while (re-search-forward pattern nil t)
      (setq match (match-string-no-properties 1))
      (unless (equal match "Pattern")
        (let ((meta (match-string-no-properties 2)))
          (when (and meta (not (string= match meta)))
            (setq meta (company-clang--strip-formatting meta))
            (when (and (not objc) (string-match "\\((.*)\\)" meta))
              (setq match (concat match (match-string 1 meta))))
            (puthash match meta company-clang--meta-cache)))
        (push match lines)))
    lines))

(defun company-clang--strip-formatting (text)
  (replace-regexp-in-string
   "#]" " "
   (replace-regexp-in-string "[<{[]#\\|#[>}]" "" text t)
   t))

(defun company-clang--handle-error (res args)
  (goto-char (point-min))
  (let* ((buf (get-buffer-create company-clang--error-buffer-name))
         (cmd (concat company-clang-executable (mapconcat 'identity args " ")))
         (pattern (format company-clang--completion-pattern ""))
         (err (if (re-search-forward pattern nil t)
                  (buffer-substring-no-properties (point-min)
                                                  (1- (match-beginning 0)))
                ;; Warn the user more agressively if no match was found.
                (message "clang failed with error %d:\n%s" res cmd)
                (buffer-string))))

    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (current-time-string)
                (format "\nclang failed with error %d:\n" res)
                cmd "\n\n")
        (insert err)
        (setq buffer-read-only t)
        (goto-char (point-min))))))

(defun company-clang--call-process (prefix &rest args)
  (let ((objc (derived-mode-p 'objc-mode)))
    (with-temp-buffer
      (let ((res (apply 'call-process company-clang-executable nil t nil args)))
        (unless (eq 0 res)
          (company-clang--handle-error res args))
        ;; Still try to get any useful input.
        (company-clang--parse-output prefix objc)))))

(defsubst company-clang--build-location (pos)
  (save-excursion
    (goto-char pos)
    (format "%s:%d:%d" buffer-file-name (line-number-at-pos)
            (1+ (current-column)))))

(defsubst company-clang--build-complete-args (pos)
  (append '("-cc1" "-fsyntax-only" "-code-completion-macros")
          company-clang-arguments
          (when (stringp company-clang--prefix)
            (list "-include" (expand-file-name company-clang--prefix)))
          '("-code-completion-at")
          (list (company-clang--build-location pos))
          (list buffer-file-name)))

(defun company-clang--candidates (prefix)
  (and company-clang-auto-save
       (buffer-modified-p)
       (basic-save-buffer))
  (when (null company-clang--prefix)
    (company-clang-set-prefix (or (funcall company-clang-prefix-guesser)
                                  'none)))
  (apply 'company-clang--call-process
         prefix
         (company-clang--build-complete-args (- (point) (length prefix)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst company-clang-required-version 1.1)

(defsubst company-clang-version ()
  "Return the version of `company-clang-executable'."
  (with-temp-buffer
    (call-process company-clang-executable nil t nil "--version")
    (goto-char (point-min))
    (if (re-search-forward "clang\\(?: version \\|-\\)\\([0-9.]+\\)" nil t)
        (let ((ver (string-to-number (match-string-no-properties 1))))
          (if (> ver 100)
              (/ ver 100)
            ver))
      0)))

(defun company-clang-objc-templatify (selector)
  (let* ((end (point-marker))
         (beg (- (point) (length selector)))
         (templ (company-template-declare-template beg end))
         (cnt 0))
    (save-excursion
      (goto-char beg)
      (catch 'stop
        (while (search-forward ":" end t)
          (company-template-add-field templ (point) (format "arg%d" cnt))
          (if (< (point) end)
              (insert " ")
            (throw 'stop t))
          (incf cnt))))
    (company-template-move-to-first templ)))

(defun company-clang (command &optional arg &rest ignored)
  "`company-mode' completion back-end for Clang.
Clang is a parser for C and ObjC.  Clang version 1.1 or newer is required.

Additional command line arguments can be specified in
`company-clang-arguments'.  Prefix files (-include ...) can be selected
with `company-clang-set-prefix' or automatically through a custom
`company-clang-prefix-guesser'.

Completions only work correctly when the buffer has been saved.
`company-clang-auto-save' determines whether to do this automatically."
  (interactive (list 'interactive))
  (case command
    (interactive (company-begin-backend 'company-clang))
    (init (when (memq major-mode company-clang-modes)
            (unless company-clang-executable
              (error "Company found no clang executable"))
            (when (< (company-clang-version) company-clang-required-version)
              (error "Company requires clang version 1.1"))))
    (prefix (and (memq major-mode company-clang-modes)
                 buffer-file-name
                 company-clang-executable
                 (not (company-in-string-or-comment))
                 (or (company-grab-symbol) 'stop)))
    (candidates (company-clang--candidates arg))
    (meta (gethash arg company-clang--meta-cache))
    (crop (and (string-match ":\\|(" arg)
               (substring arg 0 (match-beginning 0))))
    (post-completion (cond
                      ((not (derived-mode-p 'objc-mode))
                       (company-template-c-like-templatify arg))
                      ((string-match ":" arg)
                       (company-clang-objc-templatify arg))))))

(provide 'company-clang)
;;; company-clang.el ends here
