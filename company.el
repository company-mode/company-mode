;;; company.el --- Modular in-buffer completion framework  -*- lexical-binding: t -*-

;; Copyright (C) 2009-2013  Free Software Foundation, Inc.

;; Author: Nikolaj Schumacher
;; Maintainer: Dmitry Gutov <dgutov@yandex.ru>
;; Version: 0.6.8
;; Keywords: abbrev, convenience, matching
;; URL: http://company-mode.github.com/
;; Compatibility: GNU Emacs 22.x, GNU Emacs 23.x, GNU Emacs 24.x

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
;; Company is a modular completion mechanism.  Modules for retrieving completion
;; candidates are called back-ends, modules for displaying them are front-ends.
;;
;; Company comes with many back-ends, e.g. `company-elisp'.  These are
;; distributed in separate files and can be used individually.
;;
;; Place company.el and the back-ends you want to use in a directory and add the
;; following to your .emacs:
;; (add-to-list 'load-path "/path/to/company")
;; (autoload 'company-mode "company" nil t)
;;
;; Enable company-mode with M-x company-mode.  For further information look at
;; the documentation for `company-mode' (C-h f company-mode RET)
;;
;; If you want to start a specific back-end, call it interactively or use
;; `company-begin-backend'.  For example:
;; M-x company-abbrev will prompt for and insert an abbrev.
;;
;; To write your own back-end, look at the documentation for `company-backends'.
;; Here is a simple example completing "foo":
;;
;; (defun company-my-backend (command &optional arg &rest ignored)
;;   (case command
;;     (prefix (when (looking-back "foo\\>")
;;               (match-string 0)))
;;     (candidates (list "foobar" "foobaz" "foobarbaz"))
;;     (meta (format "This value is named %s" arg))))
;;
;; Sometimes it is a good idea to mix several back-ends together, for example to
;; enrich gtags with dabbrev-code results (to emulate local variables).
;; To do this, add a list with both back-ends as an element in company-backends.
;;
;; Known Issues:
;; When point is at the very end of the buffer, the pseudo-tooltip appears very
;; wrong, unless company is allowed to temporarily insert a fake newline.
;; This behavior is enabled by `company-end-of-buffer-workaround'.
;;
;;; Change Log:
;;
;; See NEWS.md in the repository.

;;; Code:

(eval-when-compile (require 'cl))

(add-to-list 'debug-ignored-errors "^.* frontend cannot be used twice$")
(add-to-list 'debug-ignored-errors "^Echo area cannot be used twice$")
(add-to-list 'debug-ignored-errors "^No \\(document\\|loc\\)ation available$")
(add-to-list 'debug-ignored-errors "^Company not ")
(add-to-list 'debug-ignored-errors "^No candidate number ")
(add-to-list 'debug-ignored-errors "^Cannot complete at point$")
(add-to-list 'debug-ignored-errors "^No other back-end$")

(defgroup company nil
  "Extensible inline text completion mechanism"
  :group 'abbrev
  :group 'convenience
  :group 'matching)

(defface company-tooltip
  '((default :foreground "black")
    (((class color) (min-colors 88) (background light))
     (:background "cornsilk"))
    (((class color) (min-colors 88) (background dark))
     (:background "yellow")))
  "Face used for the tooltip.")

(defface company-tooltip-selection
  '((default :inherit company-tooltip)
    (((class color) (min-colors 88) (background light))
     (:background "light blue"))
    (((class color) (min-colors 88) (background dark))
     (:background "orange1"))
    (t (:background "green")))
  "Face used for the selection in the tooltip.")

(defface company-tooltip-mouse
  '((default :inherit highlight))
  "Face used for the tooltip item under the mouse.")

(defface company-tooltip-common
  '((default :inherit company-tooltip)
    (((background light))
     :foreground "darkred")
    (((background dark))
     :foreground "red"))
  "Face used for the common completion in the tooltip.")

(defface company-tooltip-common-selection
  '((default :inherit company-tooltip-selection)
    (((background light))
     :foreground "darkred")
    (((background dark))
     :foreground "red"))
  "Face used for the selected common completion in the tooltip.")

(defface company-preview
  '((t :background "blue4"
       :foreground "wheat"))
  "Face used for the completion preview.")

(defface company-preview-common
  '((t :inherit company-preview
       :foreground "red"))
  "Face used for the common part of the completion preview.")

(defface company-preview-search
  '((t :inherit company-preview
       :background "blue1"))
  "Face used for the search string in the completion preview.")

(defface company-echo nil
  "Face used for completions in the echo area.")

(defface company-echo-common
  '((((background dark)) (:foreground "firebrick1"))
    (((background light)) (:background "firebrick4")))
  "Face used for the common part of completions in the echo area.")

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
  "The list of active front-ends (visualizations).
Each front-end is a function that takes one argument.  It is called with
one of the following arguments:

'show: When the visualization should start.

'hide: When the visualization should end.

'update: When the data has been updated.

'pre-command: Before every command that is executed while the
visualization is active.

'post-command: After every command that is executed while the
visualization is active.

The visualized data is stored in `company-prefix', `company-candidates',
`company-common', `company-selection', `company-point' and
`company-search-string'."
  :set 'company-frontends-set
  :type '(repeat (choice (const :tag "echo" company-echo-frontend)
                         (const :tag "echo, strip common"
                                company-echo-strip-common-frontend)
                         (const :tag "show echo meta-data in echo"
                                company-echo-metadata-frontend)
                         (const :tag "pseudo tooltip"
                                company-pseudo-tooltip-frontend)
                         (const :tag "pseudo tooltip, multiple only"
                                company-pseudo-tooltip-unless-just-one-frontend)
                         (const :tag "preview" company-preview-frontend)
                         (const :tag "preview, unique only"
                                company-preview-if-just-one-frontend)
                         (function :tag "custom function" nil))))

(defcustom company-tooltip-limit 10
  "The maximum number of candidates in the tooltip"
  :type 'integer)

(defcustom company-tooltip-minimum 6
  "The minimum height of the tooltip.
If this many lines are not available, prefer to display the tooltip above."
  :type 'integer)

(defvar company-safe-backends
  '((company-abbrev . "Abbrev")
    (company-clang . "Clang")
    (company-cmake . "CMake")
    (company-css . "CSS")
    (company-dabbrev . "dabbrev for plain text")
    (company-dabbrev-code . "dabbrev for code")
    (company-eclim . "Eclim (an Eclipse interface)")
    (company-elisp . "Emacs Lisp")
    (company-etags . "etags")
    (company-files . "Files")
    (company-gtags . "GNU Global")
    (company-ispell . "Ispell")
    (company-keywords . "Programming language keywords")
    (company-nxml . "nxml")
    (company-oddmuse . "Oddmuse")
    (company-pysmell . "PySmell")
    (company-ropemacs . "ropemacs")
    (company-semantic . "Semantic")
    (company-tempo . "Tempo templates")
    (company-xcode . "Xcode")))
(put 'company-safe-backends 'risky-local-variable t)

(defun company-safe-backends-p (backends)
  (and (consp backends)
       (not (dolist (backend backends)
              (unless (if (consp backend)
                          (company-safe-backends-p backend)
                        (assq backend company-safe-backends))
                (return t))))))

(defun company-capf (command &optional arg &rest args)
  "`company-mode' back-end using `completion-at-point-functions'.
Requires Emacs 24.1 or newer."
  (interactive (list 'interactive))
  (case command
    (interactive (company-begin-backend 'company-capf))
    (prefix
     (let ((res (run-hook-wrapped 'completion-at-point-functions
                                  ;; Ignore misbehaving functions.
                                  #'completion--capf-wrapper 'optimist)))
       (when (consp res)
         (if (> (nth 2 res) (point))
             'stop
           (buffer-substring-no-properties (nth 1 res) (point))))))
    (candidates
     (let ((res (run-hook-wrapped 'completion-at-point-functions
                                  ;; Ignore misbehaving functions.
                                  #'completion--capf-wrapper 'optimist)))
       (when (consp res)
         (all-completions arg (nth 3 res)
                          (plist-get (nthcdr 4 res) :predicate)))))))

(defcustom company-backends '(company-elisp company-nxml company-css company-cmake
                              company-semantic company-clang company-eclim
                              company-xcode company-ropemacs
                              (company-gtags company-etags company-dabbrev-code
                               company-keywords)
                              company-oddmuse company-files company-dabbrev)
  "The list of active back-ends (completion engines).
Each list elements can itself be a list of back-ends.  In that case their
completions are merged.  Otherwise only the first matching back-end returns
results.

`company-begin-backend' can be used to start a specific back-end,
`company-other-backend' will skip to the next matching back-end in the list.

Each back-end is a function that takes a variable number of arguments.
The first argument is the command requested from the back-end.  It is one
of the following:

`prefix': The back-end should return the text to be completed.  It must be
text immediately before `point'.  Returning nil passes control to the next
back-end.  The function should return 'stop if it should complete but cannot
\(e.g. if it is in the middle of a string\).  If the returned value is only
part of the prefix (e.g. the part after \"->\" in C), the back-end may return a
cons of prefix and prefix length, which is then used in the
`company-minimum-prefix-length' test.

`candidates': The second argument is the prefix to be completed.  The
return value should be a list of candidates that start with the prefix.

Optional commands:

`sorted': The back-end may return t here to indicate that the candidates
are sorted and will not need to be sorted again.

`duplicates': If non-nil, company will take care of removing duplicates
from the list.

`no-cache': Usually company doesn't ask for candidates again as completion
progresses, unless the back-end returns t for this command.  The second
argument is the latest prefix.

`meta': The second argument is a completion candidate.  The back-end should
return a (short) documentation string for it.

`doc-buffer': The second argument is a completion candidate.
The back-end should create a buffer (preferably with `company-doc-buffer'),
fill it with documentation and return it.

`location': The second argument is a completion candidate.  The back-end can
return the cons of buffer and buffer location, or of file and line
number where the completion candidate was defined.

`require-match': If this value is t, the user is not allowed to enter anything
not offered as a candidate.  Use with care!  The default value nil gives the
user that choice with `company-require-match'.  Return value 'never overrides
that option the other way around.

`init': Called once for each buffer, the back-end can check for external
programs and files and load any required libraries.  Raising an error here will
show up in message log once, and the backend will not be used for completion.

`post-completion': Called after a completion candidate has been inserted into
the buffer.  The second argument is the candidate.  Can be used to modify it,
e.g. to expand a snippet.

The back-end should return nil for all commands it does not support or
does not know about.  It should also be callable interactively and use
`company-begin-backend' to start itself in that case."
  :type `(repeat
          (choice
           :tag "Back-end"
           ,@(mapcar (lambda (b) `(const :tag ,(cdr b) ,(car b)))
                     company-safe-backends)
           (symbol :tag "User defined")
           (repeat :tag "Merged Back-ends"
                   (choice :tag "Back-end"
                           ,@(mapcar (lambda (b)
                                       `(const :tag ,(cdr b) ,(car b)))
                                     company-safe-backends)
                           (symbol :tag "User defined"))))))

(put 'company-backends 'safe-local-variable 'company-safe-backends-p)

(defcustom company-completion-started-hook nil
  "Hook run when company starts completing.
The hook is called with one argument that is non-nil if the completion was
started manually."
  :type 'hook)

(defcustom company-completion-cancelled-hook nil
  "Hook run when company cancels completing.
The hook is called with one argument that is non-nil if the completion was
aborted manually."
  :type 'hook)

(defcustom company-completion-finished-hook nil
  "Hook run when company successfully completes.
The hook is called with the selected candidate as an argument.

If you indend to use it to post-process candidates from a specific back-end,
consider using the `post-completion' command instead."
  :type 'hook)

(defcustom company-minimum-prefix-length 3
  "The minimum prefix length for automatic completion."
  :type '(integer :tag "prefix length"))

(defcustom company-require-match 'company-explicit-action-p
  "If enabled, disallow non-matching input.
This can be a function do determine if a match is required.

This can be overridden by the back-end, if it returns t or 'never to
'require-match.  `company-auto-complete' also takes precedence over this."
  :type '(choice (const :tag "Off" nil)
                 (function :tag "Predicate function")
                 (const :tag "On, if user interaction took place"
                        'company-explicit-action-p)
                 (const :tag "On" t)))

(defcustom company-auto-complete nil
  "Determines when to auto-complete.
If this is enabled, all characters from `company-auto-complete-chars' complete
the selected completion.  This can also be a function."
  :type '(choice (const :tag "Off" nil)
                 (function :tag "Predicate function")
                 (const :tag "On, if user interaction took place"
                        'company-explicit-action-p)
                 (const :tag "On" t)))

(defcustom company-auto-complete-chars '(?\  ?\) ?.)
  "Determines which characters trigger an automatic completion.
See `company-auto-complete'.  If this is a string, each string character causes
completion.  If it is a list of syntax description characters (see
`modify-syntax-entry'), all characters with that syntax auto-complete.

This can also be a function, which is called with the new input and should
return non-nil if company should auto-complete.

A character that is part of a valid candidate never triggers auto-completion."
  :type '(choice (string :tag "Characters")
                 (set :tag "Syntax"
                      (const :tag "Whitespace" ?\ )
                      (const :tag "Symbol" ?_)
                      (const :tag "Opening parentheses" ?\()
                      (const :tag "Closing parentheses" ?\))
                      (const :tag "Word constituent" ?w)
                      (const :tag "Punctuation." ?.)
                      (const :tag "String quote." ?\")
                      (const :tag "Paired delimiter." ?$)
                      (const :tag "Expression quote or prefix operator." ?\')
                      (const :tag "Comment starter." ?<)
                      (const :tag "Comment ender." ?>)
                      (const :tag "Character-quote." ?/)
                      (const :tag "Generic string fence." ?|)
                      (const :tag "Generic comment fence." ?!))
                 (function :tag "Predicate function")))

(defcustom company-idle-delay .7
  "The idle delay in seconds until automatic completions starts.
A value of nil means never complete automatically, t means complete
immediately when a prefix of `company-minimum-prefix-length' is reached."
  :type '(choice (const :tag "never (nil)" nil)
                 (const :tag "immediate (t)" t)
                 (number :tag "seconds")))

(defcustom company-begin-commands t
  "A list of commands following which company will start completing.
If this is t, it will complete after any command.  See `company-idle-delay'.

Alternatively any command with a non-nil 'company-begin property is treated as
if it was on this list."
  :type '(choice (const :tag "Any command" t)
                 (const :tag "Self insert command" '(self-insert-command))
                 (repeat :tag "Commands" function)))

(defcustom company-show-numbers nil
  "If enabled, show quick-access numbers for the first ten candidates."
  :type '(choice (const :tag "off" nil)
                 (const :tag "on" t)))

(defvar company-end-of-buffer-workaround t
  "Work around a visualization bug when completing at the end of the buffer.
The work-around consists of adding a newline.")

;;; mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar company-mode-map (make-sparse-keymap)
  "Keymap used by `company-mode'.")

(defvar company-active-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap "\e\e\e" 'company-abort)
    (define-key keymap "\C-g" 'company-abort)
    (define-key keymap (kbd "M-n") 'company-select-next)
    (define-key keymap (kbd "M-p") 'company-select-previous)
    (define-key keymap (kbd "<down>") 'company-select-next-or-abort)
    (define-key keymap (kbd "<up>") 'company-select-previous-or-abort)
    (define-key keymap [down-mouse-1] 'ignore)
    (define-key keymap [down-mouse-3] 'ignore)
    (define-key keymap [mouse-1] 'company-complete-mouse)
    (define-key keymap [mouse-3] 'company-select-mouse)
    (define-key keymap [up-mouse-1] 'ignore)
    (define-key keymap [up-mouse-3] 'ignore)
    (define-key keymap [return] 'company-complete-selection)
    (define-key keymap (kbd "RET") 'company-complete-selection)
    (define-key keymap [tab] 'company-complete-common)
    (define-key keymap (kbd "TAB") 'company-complete-common)
    (define-key keymap (kbd "<f1>") 'company-show-doc-buffer)
    (define-key keymap "\C-w" 'company-show-location)
    (define-key keymap "\C-s" 'company-search-candidates)
    (define-key keymap "\C-\M-s" 'company-filter-candidates)
    (dotimes (i 10)
      (define-key keymap (vector (+ (aref (kbd "M-0") 0) i))
        `(lambda () (interactive) (company-complete-number ,i))))

    keymap)
  "Keymap that is enabled during an active completion.")

(defvar company--disabled-backends nil)

(defun company-init-backend (backend)
  (and (symbolp backend)
       (not (fboundp backend))
       (ignore-errors (require backend nil t)))

  (if (or (symbolp backend)
          (functionp backend))
      (condition-case err
          (progn
            (funcall backend 'init)
            (put backend 'company-init t))
        (error
         (put backend 'company-init 'failed)
         (unless (memq backend company--disabled-backends)
           (message "Company back-end '%s' could not be initialized:\n%s"
                    backend (error-message-string err)))
         (pushnew backend company--disabled-backends)
         nil))
    (mapc 'company-init-backend backend)))

(defvar company-default-lighter " company")

(defvar company-lighter company-default-lighter)
(make-variable-buffer-local 'company-lighter)

;;;###autoload
(define-minor-mode company-mode
  "\"complete anything\"; is an in-buffer completion framework.
Completion starts automatically, depending on the values
`company-idle-delay' and `company-minimum-prefix-length'.

Completion can be controlled with the commands:
`company-complete-common', `company-complete-selection', `company-complete',
`company-select-next', `company-select-previous'.  If these commands are
called before `company-idle-delay', completion will also start.

Completions can be searched with `company-search-candidates' or
`company-filter-candidates'.  These can be used while completion is
inactive, as well.

The completion data is retrieved using `company-backends' and displayed using
`company-frontends'.  If you want to start a specific back-end, call it
interactively or use `company-begin-backend'.

regular keymap (`company-mode-map'):

\\{company-mode-map}
keymap during active completions (`company-active-map'):

\\{company-active-map}"
  nil company-lighter company-mode-map
  (if company-mode
      (progn
        (add-hook 'pre-command-hook 'company-pre-command nil t)
        (add-hook 'post-command-hook 'company-post-command nil t)
        (mapc 'company-init-backend company-backends))
    (remove-hook 'pre-command-hook 'company-pre-command t)
    (remove-hook 'post-command-hook 'company-post-command t)
    (company-cancel)
    (kill-local-variable 'company-point)))

;;;###autoload
(define-globalized-minor-mode global-company-mode company-mode company-mode-on)

(defun company-mode-on ()
  (unless (or noninteractive (eq (aref (buffer-name) 0) ?\s))
    (company-mode 1)))

(defsubst company-assert-enabled ()
  (unless company-mode
    (company-uninstall-map)
    (error "Company not enabled")))

;;; keymaps ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar company-my-keymap nil)
(make-variable-buffer-local 'company-my-keymap)

(defvar company-emulation-alist '((t . nil)))

(defsubst company-enable-overriding-keymap (keymap)
  (company-uninstall-map)
  (setq company-my-keymap keymap))

(defun company-ensure-emulation-alist ()
  (unless (eq 'company-emulation-alist (car emulation-mode-map-alists))
    (setq emulation-mode-map-alists
          (cons 'company-emulation-alist
                (delq 'company-emulation-alist emulation-mode-map-alists)))))

(defun company-install-map ()
  (unless (or (cdar company-emulation-alist)
              (null company-my-keymap))
    (setf (cdar company-emulation-alist) company-my-keymap)))

(defun company-uninstall-map ()
  (setf (cdar company-emulation-alist) nil))

;; Hack:
;; Emacs calculates the active keymaps before reading the event.  That means we
;; cannot change the keymap from a timer.  So we send a bogus command.
(defun company-ignore ()
  (interactive)
  (setq this-command last-command))

(global-set-key '[31415926] 'company-ignore)

(defun company-input-noop ()
  (push 31415926 unread-command-events))

;; Hack:
;; posn-col-row is incorrect in older Emacsen when line-spacing is set
(defun company--col-row (&optional pos)
  (let ((posn (posn-at-point pos)))
    (cons (car (posn-col-row posn)) (cdr (posn-actual-col-row posn)))))

(defsubst company--column (&optional pos)
  (car (posn-col-row (posn-at-point pos))))

(defsubst company--row (&optional pos)
  (cdr (posn-actual-col-row (posn-at-point pos))))

;;; backends ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun company-grab (regexp &optional expression limit)
  (when (looking-back regexp limit)
    (or (match-string-no-properties (or expression 0)) "")))

(defun company-grab-line (regexp &optional expression)
  (company-grab regexp expression (point-at-bol)))

(defun company-grab-symbol ()
  (if (looking-at "\\_>")
      (buffer-substring (point) (save-excursion (skip-syntax-backward "w_")
                                                (point)))
    (unless (and (char-after) (memq (char-syntax (char-after)) '(?w ?_)))
      "")))

(defun company-grab-word ()
  (if (looking-at "\\>")
      (buffer-substring (point) (save-excursion (skip-syntax-backward "w")
                                                (point)))
    (unless (and (char-after) (eq (char-syntax (char-after)) ?w))
      "")))

(defun company-in-string-or-comment ()
  (let ((ppss (syntax-ppss)))
    (or (car (setq ppss (nthcdr 3 ppss)))
        (car (setq ppss (cdr ppss)))
        (nth 3 ppss))))

(if (fboundp 'locate-dominating-file)
    (defalias 'company-locate-dominating-file 'locate-dominating-file)
  (defun company-locate-dominating-file (file name)
    (catch 'root
      (let ((dir (file-name-directory file))
            (prev-dir nil))
        (while (not (equal dir prev-dir))
          (when (file-exists-p (expand-file-name name dir))
            (throw 'root dir))
          (setq prev-dir dir
                dir (file-name-directory (directory-file-name dir))))))))

(defun company-call-backend (&rest args)
  (if (functionp company-backend)
      (apply company-backend args)
    (apply 'company--multi-backend-adapter company-backend args)))

(defun company--multi-backend-adapter (backends command &rest args)
  (let ((backends (loop for b in backends
                        when (not (and (symbolp b)
                                       (eq 'failed (get b 'company-init))))
                        collect b)))
    (case command
      (candidates
       (loop for backend in backends
             when (equal (funcall backend 'prefix)
                         (car args))
             append (apply backend 'candidates args)))
      (sorted nil)
      (duplicates t)
      (otherwise
       (let (value)
         (dolist (backend backends)
           (when (setq value (apply backend command args))
             (return value))))))))

;;; completion mechanism ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar company-backend nil)
(make-variable-buffer-local 'company-backend)

(defvar company-prefix nil)
(make-variable-buffer-local 'company-prefix)

(defvar company-candidates nil)
(make-variable-buffer-local 'company-candidates)

(defvar company-candidates-length nil)
(make-variable-buffer-local 'company-candidates-length)

(defvar company-candidates-cache nil)
(make-variable-buffer-local 'company-candidates-cache)

(defvar company-candidates-predicate nil)
(make-variable-buffer-local 'company-candidates-predicate)

(defvar company-common nil)
(make-variable-buffer-local 'company-common)

(defvar company-selection 0)
(make-variable-buffer-local 'company-selection)

(defvar company-selection-changed nil)
(make-variable-buffer-local 'company-selection-changed)

(defvar company--explicit-action nil
  "Non-nil, if explicit completion took place.")
(make-variable-buffer-local 'company--explicit-action)

(defvar company--auto-completion nil
  "Non-nil when current candidate is being completed automatically.
Controlled by `company-auto-complete'.")

(defvar company--point-max nil)
(make-variable-buffer-local 'company--point-max)

(defvar company-point nil)
(make-variable-buffer-local 'company-point)

(defvar company-timer nil)

(defvar company-added-newline nil)
(make-variable-buffer-local 'company-added-newline)

(defsubst company-strip-prefix (str)
  (substring str (length company-prefix)))

(defmacro company-with-candidate-inserted (candidate &rest body)
  "Evaluate BODY with CANDIDATE temporarily inserted.
This is a tool for back-ends that need candidates inserted before they
can retrieve meta-data for them."
  (declare (indent 1))
  `(let ((inhibit-modification-hooks t)
         (inhibit-point-motion-hooks t)
         (modified-p (buffer-modified-p)))
     (insert (company-strip-prefix ,candidate))
     (unwind-protect
         (progn ,@body)
       (delete-region company-point (point)))))

(defun company-explicit-action-p ()
  "Return whether explicit completion action was taken by the user."
  (or company--explicit-action
      company-selection-changed))

(defsubst company-reformat (candidate)
  ;; company-ispell needs this, because the results are always lower-case
  ;; It's mory efficient to fix it only when they are displayed.
  (concat company-prefix (substring candidate (length company-prefix))))

(defun company--should-complete ()
  (and (not (or buffer-read-only overriding-terminal-local-map
                overriding-local-map
                (minibufferp)))
       ;; Check if in the middle of entering a key combination.
       (or (equal (this-command-keys-vector) [])
           (not (keymapp (key-binding (this-command-keys-vector)))))
       (eq company-idle-delay t)
       (or (eq t company-begin-commands)
           (memq this-command company-begin-commands)
           (and (symbolp this-command) (get this-command 'company-begin)))
       (not (and transient-mark-mode mark-active))))

(defsubst company-call-frontends (command)
  (dolist (frontend company-frontends)
    (condition-case err
        (funcall frontend command)
      (error (error "Company: Front-end %s error \"%s\" on command %s"
                    frontend (error-message-string err) command)))))

(defsubst company-set-selection (selection &optional force-update)
  (setq selection (max 0 (min (1- company-candidates-length) selection)))
  (when (or force-update (not (equal selection company-selection)))
    (setq company-selection selection
          company-selection-changed t)
    (company-call-frontends 'update)))

(defun company-apply-predicate (candidates predicate)
  (let (new)
    (dolist (c candidates)
      (when (funcall predicate c)
        (push c new)))
    (nreverse new)))

(defun company-update-candidates (candidates)
  (setq company-candidates-length (length candidates))
  (if (> company-selection 0)
      ;; Try to restore the selection
      (let ((selected (nth company-selection company-candidates)))
        (setq company-selection 0
              company-candidates candidates)
        (when selected
          (while (and candidates (string< (pop candidates) selected))
            (incf company-selection))
          (unless candidates
            ;; Make sure selection isn't out of bounds.
            (setq company-selection (min (1- company-candidates-length)
                                         company-selection)))))
    (setq company-selection 0
          company-candidates candidates))
  ;; Save in cache:
  (push (cons company-prefix company-candidates) company-candidates-cache)
  ;; Calculate common.
  (let ((completion-ignore-case (company-call-backend 'ignore-case)))
    (setq company-common (company--safe-candidate
                          (try-completion company-prefix company-candidates))))
  (when (eq company-common t)
    (setq company-candidates nil)))

(defun company--safe-candidate (str)
  (or (company-call-backend 'crop str)
      str))

(defun company-calculate-candidates (prefix)
  (let ((candidates (cdr (assoc prefix company-candidates-cache)))
        (ignore-case (company-call-backend 'ignore-case)))
    (or candidates
        (when company-candidates-cache
          (let ((len (length prefix))
                (completion-ignore-case ignore-case)
                prev)
            (dotimes (i (1+ len))
              (when (setq prev (cdr (assoc (substring prefix 0 (- len i))
                                           company-candidates-cache)))
                (setq candidates (all-completions prefix prev))
                (return t)))))
        ;; no cache match, call back-end
        (progn
          (setq candidates (company-call-backend 'candidates prefix))
          (when company-candidates-predicate
            (setq candidates
                  (company-apply-predicate candidates
                                           company-candidates-predicate)))
          (unless (company-call-backend 'sorted)
            (setq candidates (sort candidates 'string<)))
          (when (company-call-backend 'duplicates)
            ;; strip duplicates
            (let ((c2 candidates))
              (while c2
                (setcdr c2 (progn (while (equal (pop c2) (car c2)))
                                  c2)))))))
    (when candidates
      (if (or (cdr candidates)
              (not (eq t (compare-strings (car candidates) nil nil
                                          prefix nil nil ignore-case))))
          candidates
        ;; Already completed and unique; don't start.
        t))))

(defun company-idle-begin (buf win tick pos)
  (and company-mode
       (eq buf (current-buffer))
       (eq win (selected-window))
       (eq tick (buffer-chars-modified-tick))
       (eq pos (point))
       (not company-candidates)
       (not (equal (point) company-point))
       (let ((company-idle-delay t)
             (company-begin-commands t))
         (company-begin)
         (when company-candidates
           (company-input-noop)
           (company-post-command)))))

(defun company-auto-begin ()
  (company-assert-enabled)
  (and company-mode
       (not company-candidates)
       (let ((company-idle-delay t)
             (company-minimum-prefix-length 0)
             (company-begin-commands t))
         (company-begin)))
  ;; Return non-nil if active.
  company-candidates)

(defun company-manual-begin ()
  (interactive)
  (setq company--explicit-action t)
  (unwind-protect
      (company-auto-begin)
    (unless company-candidates
      (setq company--explicit-action nil))))

(defun company-other-backend (&optional backward)
  (interactive (list current-prefix-arg))
  (company-assert-enabled)
  (if company-backend
      (let* ((after (cdr (member company-backend company-backends)))
             (before (cdr (member company-backend (reverse company-backends))))
             (next (if backward
                       (append before (reverse after))
                     (append after (reverse before)))))
        (company-cancel)
        (dolist (backend next)
          (when (ignore-errors (company-begin-backend backend))
            (return t))))
    (company-manual-begin))
  (unless company-candidates
    (error "No other back-end")))

(defun company-require-match-p ()
  (let ((backend-value (company-call-backend 'require-match)))
    (or (eq backend-value t)
        (and (if (functionp company-require-match)
                 (funcall company-require-match)
               (eq company-require-match t))
             (not (eq backend-value 'never))))))

(defun company-auto-complete-p (input)
  "Return non-nil, if input starts with punctuation or parentheses."
  (and (if (functionp company-auto-complete)
           (funcall company-auto-complete)
         company-auto-complete)
       (if (functionp company-auto-complete-chars)
           (funcall company-auto-complete-chars input)
         (if (consp company-auto-complete-chars)
             (memq (char-syntax (string-to-char input))
                   company-auto-complete-chars)
           (string-match (substring input 0 1) company-auto-complete-chars)))))

(defun company--incremental-p ()
  (and (> (point) company-point)
       (> (point-max) company--point-max)
       (not (eq this-command 'backward-delete-char-untabify))
       (equal (buffer-substring (- company-point (length company-prefix))
                                company-point)
              company-prefix)))

(defsubst company--string-incremental-p (old-prefix new-prefix)
  (and (> (length new-prefix) (length old-prefix))
       (equal old-prefix (substring new-prefix 0 (length old-prefix)))))

(defun company--continue-failed (new-prefix)
  (when (company--incremental-p)
    (let ((input (buffer-substring-no-properties (point) company-point)))
      (cond
       ((company-auto-complete-p input)
        ;; auto-complete
        (save-excursion
          (goto-char company-point)
          (let ((company--auto-completion t))
            (company-complete-selection))
          nil))
       ((and (company--string-incremental-p company-prefix new-prefix)
             (company-require-match-p))
        ;; wrong incremental input, but required match
        (backward-delete-char (length input))
        (ding)
        (message "Matching input is required")
        company-candidates)
       ((equal company-prefix (car company-candidates))
        ;; last input was actually success
        (company-cancel company-prefix)
        nil)))))

(defun company--good-prefix-p (prefix)
  (and (or (company-explicit-action-p)
           (unless (eq prefix 'stop)
             (>= (or (cdr-safe prefix) (length prefix))
                 company-minimum-prefix-length)))
       (stringp (or (car-safe prefix) prefix))))

(defun company--continue ()
  (when (company-call-backend 'no-cache company-prefix)
    ;; Don't complete existing candidates, fetch new ones.
    (setq company-candidates-cache nil))
  (let* ((new-prefix (company-call-backend 'prefix))
         (c (when (and (company--good-prefix-p new-prefix)
                       (setq new-prefix (or (car-safe new-prefix) new-prefix))
                       (= (- (point) (length new-prefix))
                          (- company-point (length company-prefix))))
              (setq new-prefix (or (car-safe new-prefix) new-prefix))
              (company-calculate-candidates new-prefix))))
    (or (cond
         ((eq c t)
          ;; t means complete/unique.
          (company-cancel new-prefix)
          nil)
         ((consp c)
          ;; incremental match
          (setq company-prefix new-prefix)
          (company-update-candidates c)
          c)
         (t (company--continue-failed new-prefix)))
        (company-cancel))))

(defun company--begin-new ()
  (let (prefix c)
    (dolist (backend (if company-backend
                         ;; prefer manual override
                         (list company-backend)
                       company-backends))
      (setq prefix
            (if (or (symbolp backend)
                    (functionp backend))
                (when (or (not (symbolp backend))
                          (eq t (get backend 'company-init))
                          (unless (get backend 'company-init)
                            (company-init-backend backend)))
                  (funcall backend 'prefix))
              (company--multi-backend-adapter backend 'prefix)))
      (when prefix
        (when (company--good-prefix-p prefix)
          (setq prefix (or (car-safe prefix) prefix)
                company-backend backend
                c (company-calculate-candidates prefix))
          ;; t means complete/unique.  We don't start, so no hooks.
          (if (not (consp c))
              (when company--explicit-action
                (message "No completion found"))
            (setq company-prefix prefix)
            (when (symbolp backend)
              (setq company-lighter (concat " " (symbol-name backend))))
            (company-update-candidates c)
            (run-hook-with-args 'company-completion-started-hook
                                (company-explicit-action-p))
            (company-call-frontends 'show)))
        (return c)))))

(defun company-begin ()
  (or (and company-candidates (company--continue))
      (and (company--should-complete) (company--begin-new)))
  (when company-candidates
    (when (and company-end-of-buffer-workaround (eobp))
      (save-excursion (insert "\n"))
      (setq company-added-newline (buffer-chars-modified-tick)))
    (setq company-point (point)
          company--point-max (point-max))
    (company-ensure-emulation-alist)
    (company-enable-overriding-keymap company-active-map)
    (company-call-frontends 'update)))

(defun company-cancel (&optional result)
  (and company-added-newline
       (> (point-max) (point-min))
       (let ((tick (buffer-chars-modified-tick)))
         (delete-region (1- (point-max)) (point-max))
         (equal tick company-added-newline))
       ;; Only set unmodified when tick remained the same since insert.
       (set-buffer-modified-p nil))
  (when company-prefix
    (if (stringp result)
        (progn
          (company-call-backend 'pre-completion result)
          (run-hook-with-args 'company-completion-finished-hook result)
          (company-call-backend 'post-completion result))
      (run-hook-with-args 'company-completion-cancelled-hook result)))
  (setq company-added-newline nil
        company-backend nil
        company-prefix nil
        company-candidates nil
        company-candidates-length nil
        company-candidates-cache nil
        company-candidates-predicate nil
        company-common nil
        company-selection 0
        company-selection-changed nil
        company--explicit-action nil
        company-lighter company-default-lighter
        company--point-max nil
        company-point nil)
  (when company-timer
    (cancel-timer company-timer))
  (company-search-mode 0)
  (company-call-frontends 'hide)
  (company-enable-overriding-keymap nil))

(defun company-abort ()
  (interactive)
  (company-cancel t)
  ;; Don't start again, unless started manually.
  (setq company-point (point)))

(defun company-finish (result)
  (insert (company-strip-prefix result))
  (company-cancel result)
  ;; Don't start again, unless started manually.
  (setq company-point (point)))

(defsubst company-keep (command)
  (and (symbolp command) (get command 'company-keep)))

(defun company-pre-command ()
  (unless (company-keep this-command)
    (condition-case err
        (when company-candidates
          (company-call-frontends 'pre-command))
      (error (message "Company: An error occurred in pre-command")
             (message "%s" (error-message-string err))
             (company-cancel))))
  (when company-timer
    (cancel-timer company-timer)
    (setq company-timer nil))
  (company-uninstall-map))

(defun company-post-command ()
  (unless (company-keep this-command)
    (condition-case err
        (progn
          (unless (equal (point) company-point)
            (company-begin))
          (if company-candidates
              (company-call-frontends 'post-command)
            (and (numberp company-idle-delay)
                 (or (eq t company-begin-commands)
                     (memq this-command company-begin-commands))
                 (setq company-timer
                       (run-with-timer company-idle-delay nil
                                       'company-idle-begin
                                       (current-buffer) (selected-window)
                                       (buffer-chars-modified-tick) (point))))))
      (error (message "Company: An error occurred in post-command")
             (message "%s" (error-message-string err))
             (company-cancel))))
  (company-install-map))

;;; search ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar company-search-string nil)
(make-variable-buffer-local 'company-search-string)

(defvar company-search-lighter " Search: \"\"")
(make-variable-buffer-local 'company-search-lighter)

(defvar company-search-old-map nil)
(make-variable-buffer-local 'company-search-old-map)

(defvar company-search-old-selection 0)
(make-variable-buffer-local 'company-search-old-selection)

(defun company-search (text lines)
  (let ((quoted (regexp-quote text))
        (i 0))
    (dolist (line lines)
      (when (string-match quoted line (length company-prefix))
        (return i))
      (incf i))))

(defun company-search-printing-char ()
  (interactive)
  (company-search-assert-enabled)
  (setq company-search-string
        (concat (or company-search-string "") (string last-command-event))
        company-search-lighter (concat " Search: \"" company-search-string
                                        "\""))
  (let ((pos (company-search company-search-string
                              (nthcdr company-selection company-candidates))))
    (if (null pos)
        (ding)
      (company-set-selection (+ company-selection pos) t))))

(defun company-search-repeat-forward ()
  "Repeat the incremental search in completion candidates forward."
  (interactive)
  (company-search-assert-enabled)
  (let ((pos (company-search company-search-string
                              (cdr (nthcdr company-selection
                                           company-candidates)))))
    (if (null pos)
        (ding)
      (company-set-selection (+ company-selection pos 1) t))))

(defun company-search-repeat-backward ()
  "Repeat the incremental search in completion candidates backwards."
  (interactive)
  (company-search-assert-enabled)
  (let ((pos (company-search company-search-string
                              (nthcdr (- company-candidates-length
                                         company-selection)
                                      (reverse company-candidates)))))
    (if (null pos)
        (ding)
      (company-set-selection (- company-selection pos 1) t))))

(defun company-create-match-predicate ()
  (setq company-candidates-predicate
        `(lambda (candidate)
           ,(if company-candidates-predicate
                `(and (string-match ,company-search-string candidate)
                      (funcall ,company-candidates-predicate
                               candidate))
              `(string-match ,company-search-string candidate))))
  (company-update-candidates
   (company-apply-predicate company-candidates company-candidates-predicate))
  ;; Invalidate cache.
  (setq company-candidates-cache (cons company-prefix company-candidates)))

(defun company-filter-printing-char ()
  (interactive)
  (company-search-assert-enabled)
  (company-search-printing-char)
  (company-create-match-predicate)
  (company-call-frontends 'update))

(defun company-search-kill-others ()
  "Limit the completion candidates to the ones matching the search string."
  (interactive)
  (company-search-assert-enabled)
  (company-create-match-predicate)
  (company-search-mode 0)
  (company-call-frontends 'update))

(defun company-search-abort ()
  "Abort searching the completion candidates."
  (interactive)
  (company-search-assert-enabled)
  (company-set-selection company-search-old-selection t)
  (company-search-mode 0))

(defun company-search-other-char ()
  (interactive)
  (company-search-assert-enabled)
  (company-search-mode 0)
  (company--unread-last-input))

(defvar company-search-map
  (let ((i 0)
        (keymap (make-keymap)))
    (if (fboundp 'max-char)
        (set-char-table-range (nth 1 keymap) (cons #x100 (max-char))
                              'company-search-printing-char)
      (with-no-warnings
        ;; obsolete in Emacs 23
        (let ((l (generic-character-list))
              (table (nth 1 keymap)))
          (while l
            (set-char-table-default table (car l) 'company-search-printing-char)
            (setq l (cdr l))))))
    (define-key keymap [t] 'company-search-other-char)
    (while (< i ?\s)
      (define-key keymap (make-string 1 i) 'company-search-other-char)
      (incf i))
    (while (< i 256)
      (define-key keymap (vector i) 'company-search-printing-char)
      (incf i))
    (let ((meta-map (make-sparse-keymap)))
      (define-key keymap (char-to-string meta-prefix-char) meta-map)
      (define-key keymap [escape] meta-map))
    (define-key keymap (vector meta-prefix-char t) 'company-search-other-char)
    (define-key keymap "\e\e\e" 'company-search-other-char)
    (define-key keymap  [escape escape escape] 'company-search-other-char)

    (define-key keymap "\C-g" 'company-search-abort)
    (define-key keymap "\C-s" 'company-search-repeat-forward)
    (define-key keymap "\C-r" 'company-search-repeat-backward)
    (define-key keymap "\C-o" 'company-search-kill-others)
    keymap)
  "Keymap used for incrementally searching the completion candidates.")

(define-minor-mode company-search-mode
  "Search mode for completion candidates.
Don't start this directly, use `company-search-candidates' or
`company-filter-candidates'."
  nil company-search-lighter nil
  (if company-search-mode
      (if (company-manual-begin)
          (progn
            (setq company-search-old-selection company-selection)
            (company-call-frontends 'update))
        (setq company-search-mode nil))
    (kill-local-variable 'company-search-string)
    (kill-local-variable 'company-search-lighter)
    (kill-local-variable 'company-search-old-selection)
    (company-enable-overriding-keymap company-active-map)))

(defsubst company-search-assert-enabled ()
  (company-assert-enabled)
  (unless company-search-mode
    (company-uninstall-map)
    (error "Company not in search mode")))

(defun company-search-candidates ()
  "Start searching the completion candidates incrementally.

\\<company-search-map>Search can be controlled with the commands:
- `company-search-repeat-forward' (\\[company-search-repeat-forward])
- `company-search-repeat-backward' (\\[company-search-repeat-backward])
- `company-search-abort' (\\[company-search-abort])

Regular characters are appended to the search string.

The command `company-search-kill-others' (\\[company-search-kill-others]) uses
 the search string to limit the completion candidates."
  (interactive)
  (company-search-mode 1)
  (company-enable-overriding-keymap company-search-map))

(defvar company-filter-map
  (let ((keymap (make-keymap)))
    (define-key keymap [remap company-search-printing-char]
      'company-filter-printing-char)
    (set-keymap-parent keymap company-search-map)
    keymap)
  "Keymap used for incrementally searching the completion candidates.")

(defun company-filter-candidates ()
  "Start filtering the completion candidates incrementally.
This works the same way as `company-search-candidates' immediately
followed by `company-search-kill-others' after each input."
  (interactive)
  (company-search-mode 1)
  (company-enable-overriding-keymap company-filter-map))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun company-select-next ()
  "Select the next candidate in the list."
  (interactive)
  (when (company-manual-begin)
    (company-set-selection (1+ company-selection))))

(defun company-select-previous ()
  "Select the previous candidate in the list."
  (interactive)
  (when (company-manual-begin)
    (company-set-selection (1- company-selection))))

(defun company-select-next-or-abort ()
  "Select the next candidate if more than one, else abort
and invoke the normal binding."
  (interactive)
  (if (> company-candidates-length 1)
      (company-select-next)
    (company-abort)
    (company--unread-last-input)))

(defun company-select-previous-or-abort ()
  "Select the previous candidate if more than one, else abort
and invoke the normal binding."
  (interactive)
  (if (> company-candidates-length 1)
      (company-select-previous)
    (company-abort)
    (company--unread-last-input)))

(defun company-select-mouse (event)
  "Select the candidate picked by the mouse."
  (interactive "e")
  (when (nth 4 (event-start event))
    (company-set-selection (- (cdr (posn-actual-col-row (event-start event)))
                              (company--row)
                              1))
    t))

(defun company-complete-mouse (event)
  "Complete the candidate picked by the mouse."
  (interactive "e")
  (when (company-select-mouse event)
    (company-complete-selection)))

(defun company-complete-selection ()
  "Complete the selected candidate."
  (interactive)
  (when (company-manual-begin)
    (let ((result (nth company-selection company-candidates)))
      (when company--auto-completion
        (setq result (company--safe-candidate result)))
      (company-finish result))))

(defun company-complete-common ()
  "Complete the common part of all candidates."
  (interactive)
  (when (company-manual-begin)
    (if (and (not (cdr company-candidates))
             (equal company-common (car company-candidates)))
        (company-complete-selection)
      (insert (company-strip-prefix company-common)))))

(defun company-complete ()
  "Complete the common part of all candidates or the current selection.
The first time this is called, the common part is completed, the second time, or
when the selection has been changed, the selected candidate is completed."
  (interactive)
  (when (company-manual-begin)
    (if (or company-selection-changed
            (eq last-command 'company-complete-common))
        (call-interactively 'company-complete-selection)
      (call-interactively 'company-complete-common)
      (setq this-command 'company-complete-common))))

(defun company-complete-number (n)
  "Complete the Nth candidate.
To show the number next to the candidates in some back-ends, enable
`company-show-numbers'."
  (when (company-manual-begin)
    (and (< n 1) (> n company-candidates-length)
         (error "No candidate number %d" n))
    (decf n)
    (company-finish (nth n company-candidates))))

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
  (if (> from (string-width str))
      ""
    (with-temp-buffer
      (insert str)
      (move-to-column from)
      (let ((beg (point)))
        (if to
            (progn
              (move-to-column to)
              (concat (buffer-substring beg (point))
                      (let ((padding (- to (current-column))))
                        (when (> padding 0)
                          (company-space-string padding)))))
          (buffer-substring beg (point-max)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar company-last-metadata nil)
(make-variable-buffer-local 'company-last-metadata)

(defun company-fetch-metadata ()
  (let ((selected (nth company-selection company-candidates)))
    (unless (equal selected (car company-last-metadata))
      (setq company-last-metadata
            (cons selected (company-call-backend 'meta selected))))
    (cdr company-last-metadata)))

(defun company-doc-buffer (&optional string)
  (with-current-buffer (get-buffer-create "*Company meta-data*")
    (erase-buffer)
    (current-buffer)))

(defvar company--electric-commands
  '(scroll-other-window scroll-other-window-down)
  "List of Commands that won't break out of electric commands.")

(defmacro company--electric-do (&rest body)
  (declare (indent 0) (debug t))
  `(when (company-manual-begin)
     (save-window-excursion
       (let ((height (window-height))
             (row (company--row))
             cmd)
         ,@body
         (and (< (window-height) height)
              (< (- (window-height) row 2) company-tooltip-limit)
              (recenter (- (window-height) row 2)))
         (while (memq (setq cmd (key-binding (vector (list (read-event)))))
                      company--electric-commands)
           (call-interactively cmd))
         (company--unread-last-input)))))

(defun company--unread-last-input ()
  (when last-input-event
    (clear-this-command-keys t)
    (setq unread-command-events (list last-input-event))))

(defun company-show-doc-buffer ()
  "Temporarily show a buffer with the complete documentation for the selection."
  (interactive)
  (company--electric-do
    (let* ((selected (nth company-selection company-candidates))
           (doc-buffer (or (company-call-backend 'doc-buffer selected)
                           (error "No documentation available"))))
      (with-current-buffer doc-buffer
        (goto-char (point-min)))
      (display-buffer doc-buffer t))))
(put 'company-show-doc-buffer 'company-keep t)

(defun company-show-location ()
  "Temporarily display a buffer showing the selected candidate in context."
  (interactive)
  (company--electric-do
    (let* ((selected (nth company-selection company-candidates))
           (location (company-call-backend 'location selected))
           (pos (or (cdr location) (error "No location available")))
           (buffer (or (and (bufferp (car location)) (car location))
                       (find-file-noselect (car location) t))))
      (with-selected-window (display-buffer buffer t)
        (save-restriction
          (widen)
          (if (bufferp (car location))
              (goto-char pos)
            (goto-char (point-min))
            (forward-line (1- pos))))
        (set-window-start nil (point))))))
(put 'company-show-location 'company-keep t)

;;; package functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar company-callback nil)
(make-variable-buffer-local 'company-callback)

(defvar company-begin-with-marker nil)
(make-variable-buffer-local 'company-begin-with-marker)

(defun company-remove-callback (&optional ignored)
  (remove-hook 'company-completion-finished-hook company-callback t)
  (remove-hook 'company-completion-cancelled-hook 'company-remove-callback t)
  (remove-hook 'company-completion-finished-hook 'company-remove-callback t)
  (when company-begin-with-marker
    (set-marker company-begin-with-marker nil)))

(defun company-begin-backend (backend &optional callback)
  "Start a completion at point using BACKEND."
  (interactive (let ((val (completing-read "Company back-end: "
                                           obarray
                                           'functionp nil "company-")))
                 (when val
                   (list (intern val)))))
  (when (setq company-callback callback)
    (add-hook 'company-completion-finished-hook company-callback nil t))
  (add-hook 'company-completion-cancelled-hook 'company-remove-callback nil t)
  (add-hook 'company-completion-finished-hook 'company-remove-callback nil t)
  (setq company-backend backend)
  ;; Return non-nil if active.
  (or (company-manual-begin)
      (progn
        (setq company-backend nil)
        (error "Cannot complete at point"))))

(defun company-begin-with (candidates
                           &optional prefix-length require-match callback)
  "Start a completion at point.
CANDIDATES is the list of candidates to use and PREFIX-LENGTH is the length of
the prefix that already is in the buffer before point.  It defaults to 0.

CALLBACK is a function called with the selected result if the user successfully
completes the input.

Example:
\(company-begin-with '\(\"foo\" \"foobar\" \"foobarbaz\"\)\)"
  (setq company-begin-with-marker (copy-marker (point) t))
  (company-begin-backend
   `(lambda (command &optional arg &rest ignored)
      (cond
       ((eq command 'prefix)
        (when (equal (point) (marker-position company-begin-with-marker))
          (buffer-substring ,(- (point) (or prefix-length 0)) (point))))
       ((eq command 'candidates)
        (all-completions arg ',candidates))
       ((eq command 'require-match)
        ,require-match)))
   callback))

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
      (decf company-tooltip-offset)
      (when (<= company-tooltip-offset 1)
        (setq company-tooltip-offset 0)
        (incf limit))))

  limit)

;;; propertize

(defsubst company-round-tab (arg)
  (* (/ (+ arg tab-width) tab-width) tab-width))

(defun company-untabify (str)
  (let* ((pieces (split-string str "\t"))
         (copy pieces))
    (while (cdr copy)
      (setcar copy (company-safe-substring
                    (car copy) 0 (company-round-tab (string-width (car copy)))))
      (pop copy))
    (apply 'concat pieces)))

(defun company-fill-propertize (line width selected)
  (setq line (company-safe-substring line 0 width))
  (add-text-properties 0 width '(face company-tooltip
                                 mouse-face company-tooltip-mouse)
                       line)
  (add-text-properties 0 (length company-common)
                       '(face company-tooltip-common
                         mouse-face company-tooltip-mouse)
                       line)
  (when selected
    (if (and company-search-string
             (string-match (regexp-quote company-search-string) line
                           (length company-prefix)))
        (progn
          (add-text-properties (match-beginning 0) (match-end 0)
                               '(face company-tooltip-selection)
                               line)
          (when (< (match-beginning 0) (length company-common))
            (add-text-properties (match-beginning 0) (length company-common)
                                 '(face company-tooltip-common-selection)
                                 line)))
      (add-text-properties 0 width '(face company-tooltip-selection
                                          mouse-face company-tooltip-selection)
                           line)
      (add-text-properties 0 (length company-common)
                           '(face company-tooltip-common-selection
                             mouse-face company-tooltip-selection)
                           line)))
  line)

;;; replace

(defun company-buffer-lines (beg end)
  (goto-char beg)
  (let (lines)
    (while (and (= 1 (vertical-motion 1))
                (<= (point) end))
      (push (buffer-substring beg (min end (1- (point)))) lines)
      (setq beg (point)))
    (unless (eq beg end)
      (push (buffer-substring beg end) lines))
    (nreverse lines)))

(defsubst company-modify-line (old new offset)
  (concat (company-safe-substring old 0 offset)
          new
          (company-safe-substring old (+ offset (length new)))))

(defsubst company--length-limit (lst limit)
  (if (nthcdr limit lst)
      limit
    (length lst)))

(defun company--replacement-string (lines old column nl &optional align-top)

  (let ((width (length (car lines))))
    (when (> width (- (window-width) column))
      (setq column (max 0 (- (window-width) width)))))

  (let (new)
    (when align-top
      ;; untouched lines first
      (dotimes (i (- (length old) (length lines)))
        (push (pop old) new)))
    ;; length into old lines.
    (while old
      (push (company-modify-line (pop old) (pop lines) column) new))
    ;; Append whole new lines.
    (while lines
      (push (concat (company-space-string column) (pop lines)) new))

    (let ((str (concat (when nl "\n")
                       (mapconcat 'identity (nreverse new) "\n")
                       "\n")))
      (font-lock-append-text-property 0 (length str) 'face 'default str)
      str)))

(defun company--create-lines (selection limit)

  (let ((len company-candidates-length)
        (numbered 99999)
        lines
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
    (setq width (max (length previous) (length remainder))
          lines (nthcdr company-tooltip-offset company-candidates)
          len (min limit len)
          lines-copy lines)

    (dotimes (i len)
      (setq width (max (length (pop lines-copy)) width)))
    (setq width (min width (window-width)))

    (setq lines-copy lines)

    ;; number can make tooltip too long
    (when company-show-numbers
      (setq numbered company-tooltip-offset))

    (when previous
      (push (propertize (company-safe-substring previous 0 width)
                        'face 'company-tooltip)
            new))

    (dotimes (i len)
      (push (company-fill-propertize
             (if (>= numbered 10)
                 (company-reformat (pop lines))
               (incf numbered)
               (format "%s %d"
                       (company-safe-substring (company-reformat (pop lines))
                                               0 (- width 2))
                       (mod numbered 10)))
             width (equal i selection))
            new))

    (when remainder
      (push (propertize (company-safe-substring remainder 0 width)
                        'face 'company-tooltip)
            new))

    (setq lines (nreverse new))))

;; show

(defsubst company--window-inner-height ()
  (let ((edges (window-inside-edges (selected-window))))
    (- (nth 3 edges) (nth 1 edges))))

(defsubst company--pseudo-tooltip-height ()
  "Calculate the appropriate tooltip height.
Returns a negative number if the tooltip should be displayed above point."
  (let* ((lines (company--row))
         (below (- (company--window-inner-height) 1 lines)))
    (if (and (< below (min company-tooltip-minimum company-candidates-length))
             (> lines below))
        (- (max 3 (min company-tooltip-limit lines)))
      (max 3 (min company-tooltip-limit below)))))

(defun company-pseudo-tooltip-show (row column selection)
  (company-pseudo-tooltip-hide)
  (save-excursion

    (move-to-column 0)

    (let* ((height (company--pseudo-tooltip-height))
           above)

      (when (< height 0)
        (setq row (+ row height -1)
              above t))

      (let* ((nl (< (move-to-window-line row) row))
             (beg (point))
             (end (save-excursion
                    (move-to-window-line (+ row (abs height)))
                    (point)))
             (ov (make-overlay beg end))
             (args (list (mapcar 'company-untabify
                                 (company-buffer-lines beg end))
                         column nl above)))

        (setq company-pseudo-tooltip-overlay ov)
        (overlay-put ov 'company-replacement-args args)
        (overlay-put ov 'company-before
                     (apply 'company--replacement-string
                            (company--create-lines selection (abs height))
                            args))

        (overlay-put ov 'company-column column)
        (overlay-put ov 'company-height height)))))

(defun company-pseudo-tooltip-show-at-point (pos)
  (let ((col-row (company--col-row pos)))
    (when col-row
      (company-pseudo-tooltip-show (1+ (cdr col-row)) (car col-row)
                                   company-selection))))

(defun company-pseudo-tooltip-edit (lines selection)
  (let ((column (overlay-get company-pseudo-tooltip-overlay 'company-column))
        (height (overlay-get company-pseudo-tooltip-overlay 'company-height)))
    (overlay-put company-pseudo-tooltip-overlay 'company-before
                 (apply 'company--replacement-string
                        (company--create-lines selection (abs height))
                        (overlay-get company-pseudo-tooltip-overlay
                                     'company-replacement-args)))))

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
                 (overlay-get company-pseudo-tooltip-overlay 'company-before))
    (overlay-put company-pseudo-tooltip-overlay 'window (selected-window))))

(defun company-pseudo-tooltip-guard ()
  (buffer-substring-no-properties
   (point) (overlay-start company-pseudo-tooltip-overlay)))

(defun company-pseudo-tooltip-frontend (command)
  "`company-mode' front-end similar to a tooltip but based on overlays."
  (case command
    (pre-command (company-pseudo-tooltip-hide-temporarily))
    (post-command
     (let ((old-height (if (overlayp company-pseudo-tooltip-overlay)
                           (overlay-get company-pseudo-tooltip-overlay
                                        'company-height)
                         0))
           (new-height (company--pseudo-tooltip-height)))
       (unless (and (>= (* old-height new-height) 0)
                    (>= (abs old-height) (abs new-height))
                    (equal (company-pseudo-tooltip-guard)
                           (overlay-get company-pseudo-tooltip-overlay
                                        'company-guard)))
         ;; Redraw needed.
         (company-pseudo-tooltip-show-at-point (- (point)
                                                  (length company-prefix)))
         (overlay-put company-pseudo-tooltip-overlay
                      'company-guard (company-pseudo-tooltip-guard))))
     (company-pseudo-tooltip-unhide))
    (hide (company-pseudo-tooltip-hide)
          (setq company-tooltip-offset 0))
    (update (when (overlayp company-pseudo-tooltip-overlay)
              (company-pseudo-tooltip-edit company-candidates
                                           company-selection)))))

(defun company-pseudo-tooltip-unless-just-one-frontend (command)
  "`company-pseudo-tooltip-frontend', but not shown for single candidates."
  (unless (and (eq command 'post-command)
               (not (cdr company-candidates)))
    (company-pseudo-tooltip-frontend command)))

;;; overlay ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar company-preview-overlay nil)
(make-variable-buffer-local 'company-preview-overlay)

(defun company-preview-show-at-point (pos)
  (company-preview-hide)

  (setq company-preview-overlay (make-overlay pos pos))

  (let ((completion(nth company-selection company-candidates)))
    (setq completion (propertize completion 'face 'company-preview))
    (add-text-properties 0 (length company-common)
                         '(face company-preview-common) completion)

    ;; Add search string
    (and company-search-string
         (string-match (regexp-quote company-search-string) completion)
         (add-text-properties (match-beginning 0)
                              (match-end 0)
                              '(face company-preview-search)
                              completion))

    (setq completion (company-strip-prefix completion))

    (and (equal pos (point))
         (not (equal completion ""))
         (add-text-properties 0 1 '(cursor t) completion))

    (overlay-put company-preview-overlay 'after-string completion)
    (overlay-put company-preview-overlay 'window (selected-window))))

(defun company-preview-hide ()
  (when company-preview-overlay
    (delete-overlay company-preview-overlay)
    (setq company-preview-overlay nil)))

(defun company-preview-frontend (command)
  "`company-mode' front-end showing the selection as if it had been inserted."
  (case command
    (pre-command (company-preview-hide))
    (post-command (company-preview-show-at-point (point)))
    (hide (company-preview-hide))))

(defun company-preview-if-just-one-frontend (command)
  "`company-preview-frontend', but only shown for single candidates."
  (unless (and (eq command 'post-command)
               (cdr company-candidates))
    (company-preview-frontend command)))

;;; echo ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar company-echo-last-msg nil)
(make-variable-buffer-local 'company-echo-last-msg)

(defvar company-echo-timer nil)

(defvar company-echo-delay .01)

(defun company-echo-show (&optional getter)
  (when getter
    (setq company-echo-last-msg (funcall getter)))
  (let ((message-log-max nil))
    (if company-echo-last-msg
        (message "%s" company-echo-last-msg)
      (message ""))))

(defsubst company-echo-show-soon (&optional getter)
  (when company-echo-timer
    (cancel-timer company-echo-timer))
  (setq company-echo-timer (run-with-timer 0 nil 'company-echo-show getter)))

(defsubst company-echo-show-when-idle (&optional getter)
  (when (sit-for .01)
    (company-echo-show getter)))

(defsubst company-echo-show-when-not-busy (&optional getter)
  "Run `company-echo-show' with arg GETTER once Emacs isn't busy."
  (when (sit-for company-echo-delay)
    (company-echo-show getter)))

(defun company-echo-format ()

  (let ((limit (window-width (minibuffer-window)))
        (len -1)
        ;; Roll to selection.
        (candidates (nthcdr company-selection company-candidates))
        (i (if company-show-numbers company-selection 99999))
        comp msg)

    (while candidates
      (setq comp (company-reformat (pop candidates))
            len (+ len 1 (length comp)))
      (if (< i 10)
          ;; Add number.
          (progn
            (setq comp (propertize (format "%d: %s" i comp)
                                   'face 'company-echo))
            (incf len 3)
            (incf i)
            (add-text-properties 3 (+ 3 (length company-common))
                                 '(face company-echo-common) comp))
        (setq comp (propertize comp 'face 'company-echo))
        (add-text-properties 0 (length company-common)
                             '(face company-echo-common) comp))
      (if (>= len limit)
          (setq candidates nil)
        (push comp msg)))

    (mapconcat 'identity (nreverse msg) " ")))

(defun company-echo-strip-common-format ()

  (let ((limit (window-width (minibuffer-window)))
        (len (+ (length company-prefix) 2))
        ;; Roll to selection.
        (candidates (nthcdr company-selection company-candidates))
        (i (if company-show-numbers company-selection 99999))
        msg comp)

    (while candidates
      (setq comp (company-strip-prefix (pop candidates))
            len (+ len 2 (length comp)))
      (when (< i 10)
        ;; Add number.
        (setq comp (format "%s (%d)" comp i))
        (incf len 4)
        (incf i))
      (if (>= len limit)
          (setq candidates nil)
        (push (propertize comp 'face 'company-echo) msg)))

    (concat (propertize company-prefix 'face 'company-echo-common) "{"
            (mapconcat 'identity (nreverse msg) ", ")
            "}")))

(defun company-echo-hide ()
  (unless (equal company-echo-last-msg "")
    (setq company-echo-last-msg "")
    (company-echo-show)))

(defun company-echo-frontend (command)
  "`company-mode' front-end showing the candidates in the echo area."
  (case command
    (post-command (company-echo-show-soon 'company-echo-format))
    (hide (company-echo-hide))))

(defun company-echo-strip-common-frontend (command)
  "`company-mode' front-end showing the candidates in the echo area."
  (case command
    (post-command (company-echo-show-soon 'company-echo-strip-common-format))
    (hide (company-echo-hide))))

(defun company-echo-metadata-frontend (command)
  "`company-mode' front-end showing the documentation in the echo area."
  (case command
    (post-command (company-echo-show-when-idle 'company-fetch-metadata))
    (hide (company-echo-hide))))

(provide 'company)
;;; company.el ends here
