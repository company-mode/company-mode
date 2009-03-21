;;; company.el --- extensible inline text completion mechanism
;;
;; Copyright (C) 2009 Nikolaj Schumacher
;;
;; Author: Nikolaj Schumacher <bugs * nschum de>
;; Version: 0.1
;; Keywords: abbrev, convenience, matchis
;; URL: http://nschum.de/src/emacs/company/
;; Compatibility: GNU Emacs 22.x, GNU Emacs 23.x
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
;; Company is a modular completion mechanism.  Modules for retrieving completion
;; candidates are called back-ends, modules for displaying them are front-ends.
;;
;; Company comes with many back-ends, e.g. `company-elisp'.  These are
;; distributed in individual files and can be used individually.
;;
;; Place company.el and the back-ends you want to use in a directory and add the
;; following to your .emacs:
;; (add-to-list 'load-path "/path/to/company")
;; (autoload 'company-mode "company" nil t)
;;
;; Enable company-mode with M-x company-mode.  For further information look at
;; the documentation for `company-mode' (C-h f company-mode RET)
;;
;; To write your own back-end, look at the documentation for `company-backends'.
;; Here is a simple example completing "foo":
;;
;; (defun company-my-backend (command &optional arg &rest ignored)
;;   (case command
;;     ('prefix (when (looking-back "foo\\>")
;;                (match-string 0)))
;;     ('candidates (list "foobar" "foobaz" "foobarbaz"))
;;     ('meta (format "This value is named %s" arg))))
;;
;; Known Issues:
;; When point is at the very end of the buffer, the pseudo-tooltip appears very
;; wrong.
;;
;;; Change Log:
;;
;;    Added `company-echo-strip-common-frontend'.
;;    Added `company-show-numbers' option and M-0 ... M-9 default bindings.
;;    Don't hide the echo message if it isn't shown.
;;
;; 2009-03-20 (0.1)
;;    Initial release.
;;
;;; Code:

(eval-when-compile (require 'cl))

(add-to-list 'debug-ignored-errors
             "^Pseudo tooltip frontend cannot be used twice$")
(add-to-list 'debug-ignored-errors "^Preview frontend cannot be used twice$")
(add-to-list 'debug-ignored-errors "^Echo area cannot be used twice$")
(add-to-list 'debug-ignored-errors "^No documentation available$")
(add-to-list 'debug-ignored-errors "^Company not enabled$")
(add-to-list 'debug-ignored-errors "^Company not in search mode$")
(add-to-list 'debug-ignored-errors "^No candidate number ")

(defgroup company nil
  "Extensible inline text completion mechanism"
  :group 'abbrev
  :group 'convenience
  :group 'maching)

(defface company-tooltip
  '((t :background "yellow"
       :foreground "black"))
  "*Face used for the tool tip."
  :group 'company)

(defface company-tooltip-selection
  '((default :inherit company-tooltip)
    (((class color) (min-colors 88)) (:background "orange1"))
    (t (:background "green")))
  "*Face used for the selection in the tool tip."
  :group 'company)

(defface company-tooltip-common
  '((t :inherit company-tooltip
       :foreground "red"))
  "*Face used for the common completion in the tool tip."
  :group 'company)

(defface company-tooltip-common-selection
  '((t :inherit company-tooltip-selection
       :foreground "red"))
  "*Face used for the selected common completion in the tool tip."
  :group 'company)

(defcustom company-tooltip-limit 10
  "*The maximum number of candidates in the tool tip"
  :group 'company
  :type 'integer)

(defface company-preview
  '((t :background "blue4"
       :foreground "wheat"))
  "*Face used for the completion preview."
  :group 'company)

(defface company-preview-common
  '((t :inherit company-preview
       :foreground "red"))
  "*Face used for the common part of the completion preview."
  :group 'company)

(defface company-echo nil
  "*Face used for completions in the echo area."
  :group 'company)

(defface company-echo-common
  '((((background dark)) (:foreground "firebrick1"))
    (((background light)) (:background "firebrick4")))
  "*Face used for the common part of completions in the echo area."
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
  "*The list of active front-ends (visualizations).
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
  :group 'company
  :type '(repeat (choice (const :tag "echo" company-echo-frontend)
                         (const :tag "echo, strip common"
                                company-echo-strip-common-frontend)
                         (const :tag "pseudo tooltip"
                                company-pseudo-tooltip-frontend)
                         (const :tag "pseudo tooltip, multiple only"
                                company-pseudo-tooltip-unless-just-one-frontend)
                         (const :tag "preview" company-preview-frontend)
                         (const :tag "preview, unique only"
                                company-preview-if-just-one-frontend)
                         (function :tag "custom function" nil))))

(defcustom company-backends '(company-elisp company-nxml company-css
                              company-semantic company-gtags company-oddmuse
                              company-files company-dabbrev)
  "*The list of active back-ends (completion engines).
Each back-end is a function that takes a variable number of arguments.
The first argument is the command requested from the back-end.  It is one
of the following:

'prefix: The back-end should return the text to be completed.  It must be
text immediately before `point'.  Returning nil passes control to the next
back-end.

'candidates: The second argument is the prefix to be completed.  The
return value should be a list of candidates that start with the prefix.

Optional commands:

'sorted: The back-end may return t here to indicate that the candidates
are sorted and will not need to be sorted again.

'no-cache: Usually company doesn't ask for candidates again as completion
progresses, unless the back-end returns t for this command.  The second
argument is the latest prefix.

'meta: The second argument is a completion candidate.  The back-end should
return a (short) documentation string for it.

'doc-buffer: The second argument is a completion candidate.  The back-end should
create a buffer (preferably with `company-doc-buffer'), fill it with
documentation and return it.

The back-end should return nil for all commands it does not support or
does not know about."
  :group 'company
  :type '(repeat (function :tag "function" nil)))

(defcustom company-minimum-prefix-length 3
  "*The minimum prefix length for automatic completion."
  :group 'company
  :type '(integer :tag "prefix length"))

(defcustom company-idle-delay .7
  "*The idle delay in seconds until automatic completions starts.
A value of nil means never complete automatically, t means complete
immediately when a prefix of `company-minimum-prefix-length' is reached."
  :group 'company
  :type '(choice (const :tag "never (nil)" nil)
                 (const :tag "immediate (t)" t)
                 (number :tag "seconds")))

(defcustom company-show-numbers nil
  "*If enabled, show quick-access numbers for the first ten candidates."
  :group 'company
  :type '(choice (const :tag "off" nil)
                 (const :tag "on" t)))

;;; mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar company-mode-map (make-sparse-keymap)
  "Keymap used by `company-mode'.")

(defvar company-active-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "M-n") 'company-select-next)
    (define-key keymap (kbd "M-p") 'company-select-previous)
    (define-key keymap (kbd "<down>") 'company-select-next)
    (define-key keymap (kbd "<up>") 'company-select-previous)
    (define-key keymap "\C-m" 'company-complete-selection)
    (define-key keymap "\t" 'company-complete-common)
    (define-key keymap (kbd "<f1>") 'company-show-doc-buffer)
    (define-key keymap "\C-s" 'company-search-candidates)
    (dotimes (i 10)
      (define-key keymap (vector (+ (aref (kbd "M-0") 0) i))
        `(lambda () (interactive) (company-complete-number ,i))))

    keymap)
  "Keymap that is enabled during an active completion.")

;;;###autoload
(define-minor-mode company-mode
  "\"complete anything\"; in in-buffer completion framework.
Completion starts automatically, depending on the values
`company-idle-delay' and `company-minimum-prefix-length'

Completion can be controlled with the commands:
`company-complete-common', `company-complete-selection', `company-complete',
`company-select-next', `company-select-previous'.

Completions can be searched with `company-search-candidates'.

The completion data is retrieved using `company-backends' and displayed using
`company-frontends'.

regular keymap:

\\{company-mode-map}
keymap during active completions:

\\{company-active-map}"
  nil " comp" company-mode-map
  (if company-mode
      (progn
        (add-hook 'pre-command-hook 'company-pre-command nil t)
        (add-hook 'post-command-hook 'company-post-command nil t)
        (dolist (backend company-backends)
          (unless (fboundp backend)
            (ignore-errors (require backend nil t)))
          (unless (fboundp backend)
            (message "Company back-end '%s' could not be initialized"
                     backend))))
    (remove-hook 'pre-command-hook 'company-pre-command t)
    (remove-hook 'post-command-hook 'company-post-command t)
    (company-cancel)
    (kill-local-variable 'company-point)))

;;; keymaps ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar company-overriding-keymap-bound nil)
(make-variable-buffer-local 'company-overriding-keymap-bound)

(defvar company-old-keymap nil)
(make-variable-buffer-local 'company-old-keymap)

(defvar company-my-keymap nil)
(make-variable-buffer-local 'company-my-keymap)

(defsubst company-enable-overriding-keymap (keymap)
  (setq company-my-keymap keymap)
  (when company-overriding-keymap-bound
    (company-uninstall-map)))

(defun company-install-map ()
  (unless (or company-overriding-keymap-bound
              (null company-my-keymap))
    (setq company-old-keymap overriding-terminal-local-map
          overriding-terminal-local-map company-my-keymap
          company-overriding-keymap-bound t)))

(defun company-uninstall-map ()
  (when (and company-overriding-keymap-bound
             (eq overriding-terminal-local-map company-my-keymap))
    (setq overriding-terminal-local-map company-old-keymap
          company-overriding-keymap-bound nil)))

;; Hack:
;; Emacs calculates the active keymaps before reading the event.  That means we
;; cannot change the keymap from a timer.  So we send a bogus command.
(defun company-ignore ()
  (interactive))

(global-set-key '[31415926] 'company-ignore)

(defun company-input-noop ()
  (push 31415926 unread-command-events))

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

(defvar company-point nil)
(make-variable-buffer-local 'company-point)

(defvar company-timer nil)

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
  ;; Calculate common.
  (let ((completion-ignore-case (funcall company-backend 'ignore-case)))
    (setq company-common (try-completion company-prefix company-candidates)))
  (when (eq company-common t)
    (setq company-candidates nil)))

(defsubst company-calculate-candidates (prefix)
  (setq company-prefix prefix)
  (company-update-candidates
   (or (cdr (assoc prefix company-candidates-cache))
       (when company-candidates-cache
         (let ((len (length prefix))
               (completion-ignore-case (funcall company-backend 'ignore-case))
               prev)
           (dotimes (i len)
             (when (setq prev (cdr (assoc (substring prefix 0 (- len i))
                                          company-candidates-cache)))
               (return (all-completions prefix prev))))))
       (let ((candidates (funcall company-backend 'candidates prefix)))
         (when company-candidates-predicate
           (setq candidates
                 (company-apply-predicate candidates
                                          company-candidates-predicate)))
         (unless (funcall company-backend 'sorted)
           (setq candidates (sort candidates 'string<)))
         candidates)))
  (unless (assoc prefix company-candidates-cache)
    (push (cons prefix company-candidates) company-candidates-cache))
  company-candidates)

(defun company-idle-begin (buf win tick pos)
  (and company-mode
       (eq buf (current-buffer))
       (eq win (selected-window))
       (eq tick (buffer-chars-modified-tick))
       (eq pos (point))
       (not company-candidates)
       (not (equal (point) company-point))
       (let ((company-idle-delay t))
         (company-begin)
         (when company-candidates
           (company-input-noop)
           (company-post-command)))))

(defun company-manual-begin ()
  (unless company-mode (error "Company not enabled"))
  (and company-mode
       (not company-candidates)
       (let ((company-idle-delay t)
             (company-minimum-prefix-length 0))
         (company-begin)))
  ;; Return non-nil if active.
  company-candidates)

(defun company-continue ()
  (when company-candidates
    (when (funcall company-backend 'no-cache company-prefix)
      ;; Don't complete existing candidates, fetch new ones.
      (setq company-candidates-cache nil))
    (let ((new-prefix (funcall company-backend 'prefix)))
      (unless (and (= (- (point) (length new-prefix))
                      (- company-point (length company-prefix)))
                   (or (equal company-prefix new-prefix)
                       (company-calculate-candidates new-prefix)))
        (setq company-candidates nil)))))

(defun company-begin ()
  (if (or buffer-read-only overriding-terminal-local-map overriding-local-map)
      ;; Don't complete in these cases.
      (setq company-candidates nil)
    (company-continue)
    (unless company-candidates
      (let (prefix)
        (dolist (backend company-backends)
          (and (fboundp backend)
               (setq prefix (funcall backend 'prefix))
               (company-should-complete prefix)
               (setq company-backend backend)
               (company-calculate-candidates prefix))
          (return prefix)))))
  (if company-candidates
      (progn
        (setq company-point (point))
        (company-enable-overriding-keymap company-active-map)
        (company-call-frontends 'update))
    (company-cancel)))

(defun company-cancel ()
  (setq company-backend nil
        company-prefix nil
        company-candidates nil
        company-candidates-length nil
        company-candidates-cache nil
        company-candidates-predicate nil
        company-common nil
        company-selection 0
        company-selection-changed nil
        company-point nil)
  (when company-timer
    (cancel-timer company-timer))
  (company-search-mode 0)
  (company-call-frontends 'hide)
  (company-enable-overriding-keymap nil))

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
             (company-cancel))))
  (when company-timer
    (cancel-timer company-timer))
  (company-uninstall-map))

(defun company-post-command ()
  (unless (eq this-command 'company-show-doc-buffer)
    (condition-case err
        (progn
          (unless (equal (point) company-point)
            (company-begin))
          (when company-candidates
            (company-call-frontends 'post-command))
          (when (numberp company-idle-delay)
            (setq company-timer
                  (run-with-timer company-idle-delay nil 'company-idle-begin
                                  (current-buffer) (selected-window)
                                  (buffer-chars-modified-tick) (point)))))
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
  (unless company-mode (error "Company not enabled"))
  (unless company-search-mode (error "Company not in search mode"))
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
  (unless company-mode (error "Company not enabled"))
  (unless company-search-mode (error "Company not in search mode"))
  (let ((pos (company-search company-search-string
                              (cdr (nthcdr company-selection
                                           company-candidates)))))
    (if (null pos)
        (ding)
      (company-set-selection (+ company-selection pos 1) t))))

(defun company-search-repeat-backward ()
  "Repeat the incremental search in completion candidates backwards."
  (interactive)
  (unless company-mode (error "Company not enabled"))
  (unless company-search-mode (error "Company not in search mode"))
  (let ((pos (company-search company-search-string
                              (nthcdr (- company-candidates-length
                                         company-selection)
                                      (reverse company-candidates)))))
    (if (null pos)
        (ding)
      (company-set-selection (- company-selection pos 1) t))))

(defsubst company-create-match-predicate (search-string)
  `(lambda (candidate)
     ,(if company-candidates-predicate
          `(and (string-match ,search-string candidate)
                (funcall ,company-candidates-predicate candidate))
        `(string-match ,company-search-string candidate))))

(defun company-search-kill-others ()
  "Limit the completion candidates to the ones matching the search string."
  (interactive)
  (unless company-mode (error "Company not enabled"))
  (unless company-search-mode (error "Company not in search mode"))
  (let ((predicate (company-create-match-predicate company-search-string)))
    (setq company-candidates-predicate predicate)
    (company-update-candidates (company-apply-predicate company-candidates
                                                        predicate))
    (company-search-mode 0)
    (company-call-frontends 'update)))

(defun company-search-abort ()
  "Abort searching the completion candidates."
  (interactive)
  (unless company-mode (error "Company not enabled"))
  (unless company-search-mode (error "Company not in search mode"))
  (company-set-selection company-search-old-selection t)
  (company-search-mode 0))

(defun company-search-other-char ()
  (interactive)
  (unless company-mode (error "Company not enabled"))
  (unless company-search-mode (error "Company not in search mode"))
  (company-search-mode 0)
  (when last-input-event
    (clear-this-command-keys t)
    (setq unread-command-events (list last-input-event))))

(defvar company-search-map
  (let ((i 0)
        (keymap (make-keymap)))
    (if (fboundp 'max-char)
        (set-char-table-range (nth 1 keymap) (cons #x100 (max-char))
                              'company-search-printing-char)
      (with-no-warnings
        ;; obselete in Emacs 23
        (let ((l (generic-character-list))
              (table (nth 1 keymap)))
          (while l
            (set-char-table-default table (car l) 'isearch-printing-char)
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
  "Start searching the completion candidates incrementally.

\\<company-search-map>Search can be controlled with the commands:
- `company-search-repeat-forward' (\\[company-search-repeat-forward])
- `company-search-repeat-backward' (\\[company-search-repeat-backward])
- `company-search-abort' (\\[company-search-abort])

Regular characters are appended to the search string.

The command `company-search-kill-others' (\\[company-search-kill-others]) uses
 the search string to limit the completion candidates."
  nil company-search-lighter nil
  (if company-search-mode
      (if (company-manual-begin)
          (progn
            (setq company-search-old-selection company-selection)
            (company-enable-overriding-keymap company-search-map)
            (company-call-frontends 'update))
        (setq company-search-mode nil))
    (kill-local-variable 'company-search-string)
    (kill-local-variable 'company-search-lighter)
    (kill-local-variable 'company-search-old-selection)
    (company-enable-overriding-keymap company-active-map)))

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
  (company-search-mode 1))

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

(defun company-complete-selection ()
  "Complete the selected candidate."
  (interactive)
  (when (company-manual-begin)
    (insert (company-strip-prefix (nth company-selection company-candidates)))
    (company-abort)))

(defun company-complete-common ()
  "Complete the common part of all candidates."
  (interactive)
  (when (company-manual-begin)
    (insert (company-strip-prefix company-common))))

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
  "Complete the Nth candidate."
  (when (company-manual-begin)
    (and (< n 1) (> n company-candidates-length)
         (error "No candidate number %d" n))
    (decf n)
    (insert (company-strip-prefix (nth n company-candidates)))
    (company-abort)))

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
  "Temporarily show a buffer with the complete documentation for the selection."
  (interactive)
  (unless company-mode (error "Company not enabled"))
  (when (company-manual-begin)
    (save-window-excursion
      (let* ((height (window-height))
             (row (cdr (posn-col-row (posn-at-point))))
             (selected (nth company-selection company-candidates))
             (buffer (funcall company-backend 'doc-buffer selected)))
        (if (not buffer)
            (error "No documentation available.")
          (display-buffer buffer)
          (and (< (window-height) height)
               (< (- (window-height) row 2) company-tooltip-limit)
               (recenter (- (window-height) row 2)))
          (while (eq 'scroll-other-window
                     (key-binding (vector (list (read-event)))))
            (scroll-other-window))
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
  (add-text-properties 0 width (list 'face 'company-tooltip) line)
  (add-text-properties 0 (length company-common)
                       (list 'face 'company-tooltip-common) line)
  (when selected
    (if (and company-search-string
             (string-match (regexp-quote company-search-string) line
                           (length company-prefix)))
        (progn
          (add-text-properties (match-beginning 0) (match-end 0)
                               '(face company-tooltip-selection) line)
          (when (< (match-beginning 0) (length company-common))
            (add-text-properties (match-beginning 0) (length company-common)
                                 '(face company-tooltip-common-selection)
                                 line)))
      (add-text-properties 0 width '(face company-tooltip-selection) line)
      (add-text-properties 0 (length company-common)
                           (list 'face 'company-tooltip-common-selection)
                           line)))
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

(defsubst company-modify-line (old new offset)
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
      (push (concat (company-space-string column) (pop lines)) new))
    (concat (when nl "\n")
            (mapconcat 'identity (nreverse new) "\n")
            "\n")))

(defun company-create-lines (column selection limit)

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
    (setq width (min (length previous) (length remainder))
          lines (nthcdr company-tooltip-offset company-candidates)
          len (min limit len)
          lines-copy lines)

    (dotimes (i len)
      (setq width (max (length (pop lines-copy)) width)))
    (setq width (min width (- (window-width) column)))

    (setq lines-copy lines)

    ;; number can make tooltip too long
    (and company-show-numbers
         (< (setq numbered company-tooltip-offset) 10)
         (incf width 2))

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

(defsubst company-pseudo-tooltip-height ()
  "Calculate the appropriate tooltip height."
  (max 3 (min company-tooltip-limit
              (- (window-height) 2
                 (count-lines (window-start) (point-at-bol))))))

(defun company-pseudo-tooltip-show (row column selection)
  (company-pseudo-tooltip-hide)
  (save-excursion

    (move-to-column 0)

    (let* ((height (company-pseudo-tooltip-height))
           (lines (company-create-lines column selection height))
           (nl (< (move-to-window-line row) row))
           (beg (point))
           (end (save-excursion
                  (move-to-window-line (+ row height))
                  (point)))
           (old-string
            (mapcar 'company-untabify (company-buffer-lines beg end)))
           str)

      (setq company-pseudo-tooltip-overlay (make-overlay beg end))

      (overlay-put company-pseudo-tooltip-overlay 'company-old old-string)
      (overlay-put company-pseudo-tooltip-overlay 'company-column column)
      (overlay-put company-pseudo-tooltip-overlay 'company-nl nl)
      (overlay-put company-pseudo-tooltip-overlay 'company-before
                   (company-replacement-string old-string lines column nl))
      (overlay-put company-pseudo-tooltip-overlay 'company-height height)

      (overlay-put company-pseudo-tooltip-overlay 'window (selected-window)))))

(defun company-pseudo-tooltip-show-at-point (pos)
  (let ((col-row (posn-col-row (posn-at-point pos))))
    (company-pseudo-tooltip-show (1+ (cdr col-row)) (car col-row) company-selection)))

(defun company-pseudo-tooltip-edit (lines selection)
  (let* ((old-string (overlay-get company-pseudo-tooltip-overlay 'company-old))
         (column (overlay-get company-pseudo-tooltip-overlay 'company-column))
         (nl (overlay-get company-pseudo-tooltip-overlay 'company-nl))
         (height (overlay-get company-pseudo-tooltip-overlay 'company-height))
         (lines (company-create-lines column selection height)))
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
                 (overlay-get company-pseudo-tooltip-overlay 'company-before))
    (overlay-put company-pseudo-tooltip-overlay 'window (selected-window))))

(defun company-pseudo-tooltip-frontend (command)
  "A `company-mode' front-end similar to a tool-tip but based on overlays."
  (case command
    ('pre-command (company-pseudo-tooltip-hide-temporarily))
    ('post-command
     (unless (and (overlayp company-pseudo-tooltip-overlay)
                  (equal (overlay-get company-pseudo-tooltip-overlay
                                      'company-height)
                         (company-pseudo-tooltip-height)))
       ;; Redraw needed.
       (company-pseudo-tooltip-show-at-point (- (point)
                                                (length company-prefix))))
     (company-pseudo-tooltip-unhide))
    ('hide (company-pseudo-tooltip-hide)
           (setq company-tooltip-offset 0))
    ('update (when (overlayp company-pseudo-tooltip-overlay)
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
  "A `company-mode' front-end showing the selection as if it had been inserted."
  (case command
    ('pre-command (company-preview-hide))
    ('post-command (company-preview-show-at-point (point)))
    ('hide (company-preview-hide))))

(defun company-preview-if-just-one-frontend (command)
  "`company-preview-frontend', but only shown for single candidates."
  (unless (and (eq command 'post-command)
               (cdr company-candidates))
    (company-preview-frontend command)))

;;; echo ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar company-echo-last-msg nil)
(make-variable-buffer-local 'company-echo-last-msg)

(defvar company-echo-timer nil)

(defvar company-echo-delay .1)

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
  (setq company-echo-timer (run-with-timer company-echo-delay nil
                                           'company-echo-show getter)))

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
  (when company-echo-timer
    (cancel-timer company-echo-timer))
  (unless (equal company-echo-last-msg "")
    (setq company-echo-last-msg "")
    (company-echo-show)))

(defun company-echo-frontend (command)
  "A `company-mode' front-end showing the candidates in the echo area."
  (case command
    ('pre-command (company-echo-show-soon))
    ('post-command (company-echo-show-soon 'company-echo-format))
    ('hide (company-echo-hide))))

(defun company-echo-strip-common-frontend (command)
  "A `company-mode' front-end showing the candidates in the echo area."
  (case command
    ('pre-command (company-echo-show-soon))
    ('post-command (company-echo-show-soon 'company-echo-strip-common-format))
    ('hide (company-echo-hide))))

(defun company-echo-metadata-frontend (command)
  "A `company-mode' front-end showing the documentation in the echo area."
  (case command
    ('pre-command (company-echo-show-soon))
    ('post-command (company-echo-show-soon 'company-fetch-metadata))
    ('hide (company-echo-hide))))

(provide 'company)
;;; company.el ends here
