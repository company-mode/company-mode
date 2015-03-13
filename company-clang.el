;;; company-clang.el --- company-mode completion back-end for Clang  -*- lexical-binding: t -*-

;; Copyright (C) 2009, 2011, 2013-2015  Free Software Foundation, Inc.

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
(require 'cl-lib)

(defgroup company-clang nil
  "Completion back-end for Clang."
  :group 'company)

(defcustom company-clang-executable
  (executable-find "clang")
  "Location of clang executable."
  :type 'file)

(defcustom company-clang-begin-after-member-access t
  "When non-nil, automatic completion will start whenever the current
symbol is preceded by \".\", \"->\" or \"::\", ignoring
`company-minimum-prefix-length'.

If `company-begin-commands' is a list, it should include `c-electric-lt-gt'
and `c-electric-colon', for automatic completion right after \">\" and
\":\".")

(defcustom company-clang-arguments nil
  "Additional arguments to pass to clang when completing.
Prefix files (-include ...) can be selected with `company-clang-set-prefix'
or automatically through a custom `company-clang-prefix-guesser'."
  :type '(repeat (string :tag "Argument")))

(defcustom company-clang-prefix-guesser 'company-clang-guess-prefix
  "A function to determine the prefix file for the current buffer."
  :type '(function :tag "Guesser function" nil))

(defvar company-clang-modes '(c-mode c++-mode objc-mode)
  "Major modes which clang may complete.")

(defcustom company-clang-insert-arguments t
  "When non-nil, insert function arguments as a template after completion."
  :type 'boolean
  :package-version '(company . "0.8.0"))

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

;; The option "-code-completion-brief-comments" works since Clang
;; version 3.2.  The options "-ast-dump -ast-dump-filter" are part of
;; Clang since version 3.2, but can parse comments starting from
;; version 3.3.
(defconst company-clang-parse-comments-min-version 3.3
  "Starting from version 3.3 Clang's AST can parse comments.")

(defcustom company-clang-parse-system-headers-comments nil
  "Parse completions' documentation comments of system headers.

Clang can parse only comments wrote in Doxygen style."
  :type 'boolean)

(defun company-clang--parse-AST (candidate)
  "Return the CANDIDATE's AST.

Resolve function overloads by searching the candidate's meta in
the Clang's AST.

Manage 'invalid sloc', for instance when dumping the AST of
'printf', without the need to force the c++ mode (-x c++). The
variadic argument (...) in the function declaration is known to
create this sort of problems."
  (goto-char (point-min))
  (let* ((prefix (regexp-quote candidate))
         (meta (company-clang--meta candidate))
         (head (format "^Dumping \\(?:\\(.*\\)::\\)?%s:$" prefix))
         (decl (format "^\\(.*Decl\\) .* %s '\\([^'(\n]*\\)\\([^']*)\\)?'.*$" prefix))
         (abort nil)
         head-beg head-end empty-line
         parent obj proto args variadic AST-meta type name)
    (while (not abort)
      (if (not (re-search-forward head nil t))
          (setq abort t)
        (setq parent (match-string-no-properties 1))
        (setq head-beg (match-beginning 0))
        (setq head-end (match-end 0))
        (if (not (re-search-forward "^$" nil t))
            (setq abort t)
          (setq empty-line (match-end 0))
          (goto-char (+ head-end 1))
          (when (re-search-forward decl empty-line t)
            (goto-char (+ (match-end 0) 1))
            (setq obj (match-string-no-properties 1))
            (setq proto (match-string-no-properties 2))
            (setq args (match-string-no-properties 3))
            (setq variadic (if args (string-match "^(.*[.][.][.])$" args) nil))
            (cond
             ;; Match enum's member.
             ((and (string= obj "EnumConstantDecl")
                   (or
                    ;; C language. Also works with C++ when the enum is
                    ;; anonymous.
                    (string= meta (concat
                                   "enum "
                                   (if (string= parent "") "<anonymous>" parent)
                                   " "
                                   prefix))
                    ;; C++ language. A non-anonymous enum requires this version.
                    (string= meta (concat
                                   (if (string= parent "") "<anonymous>" parent)
                                   " "
                                   prefix))))
              (setq abort 'ok))
             ;; Anonymous declaration, usually an enum or a struct.
             ((and args
                   (string-match "(anonymous [^ \n]+ at.*)" args)
                   (string= meta (concat proto "(anonymous) " prefix)))
              (setq abort 'ok))
             ;; If `args' is nil, guess it's a type or struct declaration.  In
             ;; this case `proto' is the definition of `prefix'.  This is ok when
             ;; `meta' is nil or equal to the full definition.
             ((and (not args) (or (not meta)
                                  (string= meta (concat proto " " prefix))))
              (setq abort 'ok))
             (t
              ;; Reconstruct the function declaration from the AST.
              (setq AST-meta nil)
              (while (re-search-forward
                      "^.*ParmVarDecl.* \\([^>\n]*\\) '\\([^'\n]*\\)'.*$" empty-line t)
                (setq type (match-string-no-properties 2))
                (setq name (match-string-no-properties 1))
                (setq AST-meta
                      (concat AST-meta (if AST-meta ", ")
                              type (unless (string= (substring type -1) "*") " ")
                              name)))
              (setq AST-meta (concat proto prefix
                                     "(" AST-meta (if variadic ", ...") ")"))
              (when (string= meta AST-meta)
                (setq abort 'ok)))))
          (goto-char (+ empty-line 1)))))
    (when (eq abort 'ok)
      (buffer-substring head-beg empty-line))))

(defun company-clang--can-parse-comments nil
  "Verify that the version of Clang in use can parse comments."
  (>= company-clang--version
      company-clang-parse-comments-min-version))

(defun company-clang--get-candidate-doc (candidate)
  "Extract the documentation of a CANDIDATE."
  (let (doc ast)
    (when (company-clang--can-parse-comments)
      (setq ast (company-clang--AST-process candidate))
      (setq doc (company-clang--get-ast-doc ast)))
    doc))

(defun company-clang--get-ast-doc (ast)
  "Get the AST's comments.

Return the AST's comments."
  (let (doc last-line line-begin line-end empty-lines
            col-begin last-string missing-spaces)
    (when (stringp ast)
      (with-temp-buffer
        (insert ast)
        (goto-char (point-min))
        ;; Search a paragraph.
        (while (re-search-forward "^.*ParagraphComment[^<]+<\\(?:\\(?:line:\\([0-9]+\\)\\)[^l\n]*\\(?:line:\\([0-9]+\\)\\)?\\)?.*$" nil t)
          (setq line-begin (match-string-no-properties 1))
          (setq line-end (match-string-no-properties 2))
          (when line-begin
            (setq line-begin (string-to-number line-begin)))
          ;; If `line-end' is nil, this is a single line paragraph
          ;; independently from the value of `line-begin'. If both
          ;; `line-begin' and `line-end' are non-nil, this is a
          ;; multi-line paragraph.
          (if line-end
              (setq line-end (string-to-number line-end))
            ;; Single line paragraph.
            (setq line-end line-begin))
          ;; Calculate the number of empty lines between two paragraphs.
          (when (and last-line line-end)
            (setq empty-lines (- line-begin last-line 1)))
          ;; Insert empty lines between two paragraphs.
          (while (and empty-lines (> empty-lines 0))
            (setq doc (concat doc "\n\n"))
            (setq empty-lines (- empty-lines 1)))
          (setq last-line (or line-end last-line))
          (goto-char (+ (match-end 0) 1))
          ;; Search the comments line by line.
          (let ((search-comments t)
                (previous-pos (point))
                (previous-line line-begin)
                current-line comment next-pos key value
                current-col col-diff string-length spaces-diff)
            ;; Generic comment line parsing.
            (while (and search-comments
                        (re-search-forward
                         "^[^a-zA-Z]*\\([a-zA-Z]+Comment\\)[^<]+\\(.*\\)$"
                         (point-at-eol) t))
              (setq comment nil)
              (setq next-pos (+ (match-end 0) 1))
              (setq key (match-string-no-properties 1))
              (setq value (match-string-no-properties 2))
              ;; If `current-line' is nil, we stay on the same line.
              (when (string-match "^<line:\\([0-9]+\\)" value)
                (setq current-line
                      (string-to-number (match-string 1 value))))
              ;; Find the current column.
              (when (or (string-match "^<line:[0-9]+:\\([0-9]+\\)" value)
                        (string-match "^<col:\\([0-9]+\\)" value))
                (setq current-col (string-to-number (match-string 1 value))))
              (unless col-begin
                (setq col-begin current-col))
              (cond
               ((string= key "ParagraphComment")
                ;; We reached a new paragraph, it's time to switch to
                ;; this new paragraph. There are no more comments to
                ;; parse for now.
                (setq search-comments nil)
                (goto-char previous-pos))
               ((string= key "TextComment")
                ;; We are parsing something like:
                ;; 'Text=" Create the DOCUMENT-START event."'
                (string-match "Text=\"\\(.*\\)\"$" value)
                (setq comment (match-string 1 value)))
               ((string= key "InlineCommandComment")
                ;; We are parsing something like:
                ;; 'Name="c" RenderMonospaced Arg[0]="NULL."'
                ;; The comment should be after 'Arg[0]='.
                (string-match "Arg\\[0\\]=\"\\(.*\\)\"$" value)
                (setq comment (match-string 1 value)))
               ((string= key "BlockCommandComment")
                ;; We are parsing something like:
                ;; 'Name="returns"'
                ;; A comment block begins, so we force a new-line.
                (when (string-match "Name=\"\\(.*\\)\"$" value)
                  (setq comment (concat (match-string 1 value) "\n"))))
               ((string= key "ParamCommandComment")
                ;; We are parsing something like:
                ;; '[out] explicitly Param="event" ParamIndex=0'
                ;; Each comment could be grouped under the same
                ;; comment block, 'ParamIndex=0' expresses the
                ;; position in the block.
                (when (string-match
                       "^<[^>]*> \\([^ ]+\\) .*Param=\"\\(.*\\)\".*$" value)
                  (push 'pending missing-spaces)
                  ;; @param[dir] parameter-name parameter-description
                  ;; Possible values of 'dir' are 'in', 'out', and
                  ;; 'in,out'. So, we align the value of 'Param' to
                  ;; '[in,out] '.
                  (setq comment (match-string 1 value))
                  (setq spaces-diff (- (length "[in,out] ") (length comment)))
                  (while (> spaces-diff 0)
                    (setq comment (concat comment " "))
                    (setq spaces-diff (- spaces-diff 1)))
                  (setq comment (concat comment (match-string 2 value)))))
               (t
                ;; We are dealing with an unknown `key', it is better
                ;; to write a stub.
                (when (string-match "^<[^>]*> \\(.*\\)$" value)
                  (setq comment
                        (concat "STUB[" (match-string 1 value) "]")))))
              (when search-comments
                ;; Handles new-lines and in-line comments.
                (when (and current-line doc)
                  (when (or (not previous-line)
                            (> current-line previous-line))
                    (pop missing-spaces)
                    (setq col-begin current-col)
                    (setq doc (concat doc "\n"))))
                (setq previous-line (or current-line previous-line))
                ;; Add missing spaces to the in-line comment.
                (when (and last-string comment)
                  (cond
                   ((and (= (length missing-spaces) 1 )
                         (equal (car missing-spaces) 'pending))
                    (setcar missing-spaces 'do))
                   (missing-spaces
                    (setq col-diff (- current-col col-begin))
                    (setq string-length (length last-string))
                    (setq spaces-diff (- col-diff string-length))
                    (while (> spaces-diff 0)
                      (setq comment (concat " " comment))
                      (setq spaces-diff (- spaces-diff 1)))
                    (pop missing-spaces)))
                  (setq col-begin current-col))
                (setq last-string comment)
                ;; Unhandled sistuation.
                (unless comment
                  (setq comment "[FIXME: COMMENT_PARSING_ERROR]"))
                (setq doc (concat doc comment))
                (setq previous-pos next-pos)
                (goto-char next-pos)))))))
    doc))

(defun company-clang--doc-buffer (candidate)
  "Create the documentation buffer for a CANDIDATE."
  ;; If there isn't a candidate's meta, the candidate could be a type.
  ;; In this case, use the candidate's prefix as `meta'.
  (let ((meta (or (company-clang--meta candidate) (regexp-quote candidate)))
        (doc (company-clang--get-candidate-doc candidate)))
    (when doc
      (company-doc-buffer
       (concat meta "\n\n" doc)))))

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

(defconst company-clang--error-buffer-name "*clang-error*")

(defun company-clang--lang-option ()
     (if (eq major-mode 'objc-mode)
         (if (string= "m" (file-name-extension buffer-file-name))
             "objective-c" "objective-c++")
       (substring (symbol-name major-mode) 0 -5)))

(defun company-clang--parse-output (prefix _objc)
  (goto-char (point-min))
  (let ((pattern (format company-clang--completion-pattern
                         (regexp-quote prefix)))
        (case-fold-search nil)
        lines match)
    (while (re-search-forward pattern nil t)
      (setq match (match-string-no-properties 1))
      (unless (equal match "Pattern")
        (save-match-data
          (when (string-match ":" match)
            (setq match (substring match 0 (match-beginning 0)))))
        (let ((meta (match-string-no-properties 2)))
          (when (and meta (not (string= match meta)))
            (put-text-property 0 1 'meta
                               (company-clang--strip-formatting meta)
                               match)))
        (push match lines)))
    lines))

(defun company-clang--meta (candidate)
  (get-text-property 0 'meta candidate))

(defun company-clang--annotation (candidate)
  (let ((meta (company-clang--meta candidate)))
    (cond
     ((null meta) nil)
     ((string-match "[^:]:[^:]" meta)
      (substring meta (1+ (match-beginning 0))))
     ((string-match "\\((.*)[ a-z]*\\'\\)" meta)
      (let ((paren (match-beginning 1)))
        (if (not (eq (aref meta (1- paren)) ?>))
            (match-string 1 meta)
          (with-temp-buffer
            (insert meta)
            (goto-char paren)
            (substring meta (1- (search-backward "<"))))))))))

(defun company-clang--strip-formatting (text)
  (replace-regexp-in-string
   "#]" " "
   (replace-regexp-in-string "[<{[]#\\|#[>}]" "" text t)
   t))

(defun company-clang--handle-error (res args)
  (goto-char (point-min))
  (let* ((buf (get-buffer-create company-clang--error-buffer-name))
         (cmd (concat company-clang-executable " " (mapconcat 'identity args " ")))
         (pattern (format company-clang--completion-pattern ""))
         (err (if (re-search-forward pattern nil t)
                  (buffer-substring-no-properties (point-min)
                                                  (1- (match-beginning 0)))
                ;; Warn the user more aggressively if no match was found.
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

(defun company-clang--AST-process (candidate)
  "Process the CANDIDATE's AST synchronously.

Return the CANDIDATE's AST."
  ;; NOTE: build the args while in the original buffer.
  (let* ((prefix (regexp-quote candidate))
         (args (company-clang--build-AST-args prefix))
         (buf (get-buffer-create "*clang-ast*"))
         (process-adaptive-read-buffering nil))
    (unless (get-buffer-process buf)
      (with-current-buffer buf
        (buffer-disable-undo)
        (erase-buffer))
      ;; NOTE: start the process while in the original buffer.
      (let (process)
        (setq process
              (apply #'call-process-region (point-min) (point-max)
                     company-clang-executable
                     nil (list buf nil) nil args))
        (with-current-buffer buf
          ;; FIXME: `company-clang--handle-error' seems to
          ;; create troubles some time, we should suppress
          ;; Clang's errors, in the meantime do not consider
          ;; the return code 1 as an error.
          (unless (or (eq 0 process) (eq 1 process))
            (company-clang--handle-error process args))
          (company-clang--parse-AST candidate))))))

(defun company-clang--start-process (prefix callback &rest args)
  (let ((objc (derived-mode-p 'objc-mode))
        (buf (get-buffer-create "*clang-output*"))
        ;; Looks unnecessary in Emacs 25.1 and later.
        (process-adaptive-read-buffering nil))
    (with-current-buffer buf
      (buffer-disable-undo)
      (erase-buffer))
    (if (get-buffer-process buf)
        (funcall callback nil)
      (let ((process (apply #'start-process "company-clang" buf
                            company-clang-executable args)))
        (set-process-sentinel
         process
         (lambda (proc status)
           (unless (string-match-p "hangup" status)
             (funcall
              callback
              (let ((res (process-exit-status proc)))
                (with-current-buffer buf
                  (unless (eq 0 res)
                    (company-clang--handle-error res args))
                  ;; Still try to get any useful input.
                  (company-clang--parse-output prefix objc)))))))
        (send-region process (point-min) (point-max))
        (send-string process "\n")
        (process-send-eof process)))))

(defsubst company-clang--build-location (pos)
  (save-excursion
    (goto-char pos)
    (format "-:%d:%d"
            (line-number-at-pos)
            (1+ (length
                 (encode-coding-region
                  (line-beginning-position)
                  (point)
                  'utf-8
                  t))))))

(defun company-clang--build-AST-args (prefix)
  "Return Clang's args to dump the AST filtering by PREFIX"
  (append '("-fno-color-diagnostics" "-fsyntax-only" "-w"
            "-Xclang" "-ast-dump" "-Xclang" "-ast-dump-filter"
            "-Xclang")
          (list prefix)
          (when company-clang-parse-system-headers-comments
            (list "-Xclang" "--no-system-header-prefix="))
          (list "-x" (company-clang--lang-option))
          company-clang-arguments
          (when (stringp company-clang--prefix)
            (list "-include" (expand-file-name company-clang--prefix)))
          (list "-")))

(defsubst company-clang--build-complete-args (pos)
  (append '("-fsyntax-only" "-Xclang" "-code-completion-macros")
          (list "-x" (company-clang--lang-option))
          company-clang-arguments
          (when (stringp company-clang--prefix)
            (list "-include" (expand-file-name company-clang--prefix)))
          (list "-Xclang" (format "-code-completion-at=%s"
                                  (company-clang--build-location pos)))
          (list "-")))

(defun company-clang--candidates (prefix callback)
  (and (buffer-modified-p)
       (basic-save-buffer))
  (when (null company-clang--prefix)
    (company-clang-set-prefix (or (funcall company-clang-prefix-guesser)
                                  'none)))
  (apply 'company-clang--start-process
         prefix
         callback
         (company-clang--build-complete-args (- (point) (length prefix)))))

(defun company-clang--prefix ()
  (if company-clang-begin-after-member-access
      (company-grab-symbol-cons "\\.\\|->\\|::" 2)
    (company-grab-symbol)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst company-clang-required-version 2.9)

(defvar company-clang--version nil)

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
         (beg (- (point) (length selector) 1))
         (templ (company-template-declare-template beg end))
         (cnt 0))
    (save-excursion
      (goto-char beg)
      (catch 'stop
        (while (search-forward ":" end t)
          (when (looking-at "([^)]*) ?")
            (delete-region (match-beginning 0) (match-end 0)))
          (company-template-add-field templ (point) (format "arg%d" cnt))
          (if (< (point) end)
              (insert " ")
            (throw 'stop t))
          (cl-incf cnt))))
    (company-template-move-to-first templ)))

(defun company-clang (command &optional arg &rest ignored)
  "`company-mode' completion back-end for Clang.
Clang is a parser for C and ObjC.  Clang version 2.9 or newer is required.

Additional command line arguments can be specified in
`company-clang-arguments'.  Prefix files (-include ...) can be selected
with `company-clang-set-prefix' or automatically through a custom
`company-clang-prefix-guesser'.

With Clang versions before 2.9, we have to save the buffer before
performing completion.  With Clang 2.9 and later, buffer contents are
passed via standard input."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-clang))
    (init (when (memq major-mode company-clang-modes)
            (unless company-clang-executable
              (error "Company found no clang executable"))
            (setq company-clang--version (company-clang-version))
            (when (< company-clang--version company-clang-required-version)
              (error "Company requires clang version 2.9"))))
    (prefix (and (memq major-mode company-clang-modes)
                 buffer-file-name
                 company-clang-executable
                 (not (company-in-string-or-comment))
                 (or (company-clang--prefix) 'stop)))
    (candidates (cons :async
                      (lambda (cb) (company-clang--candidates arg cb))))
    (meta       (company-clang--meta arg))
    (annotation (company-clang--annotation arg))
    (doc-buffer (unless (company-clang--can-parse-comments)
                  (error "The current version of Clang cannot parse comments"))
                (company-clang--doc-buffer arg))
    (post-completion (let ((anno (company-clang--annotation arg)))
                       (when (and company-clang-insert-arguments anno)
                         (insert anno)
                         (if (string-match "\\`:[^:]" anno)
                             (company-clang-objc-templatify anno)
                           (company-template-c-like-templatify
                            (concat arg anno))))))))

(provide 'company-clang)
;;; company-clang.el ends here
