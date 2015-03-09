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

(defcustom company-clang-parse-comments-as-c++-mode t
  "When the mode is c, force the c++ mode to dump the AST and
parse comments.

This solves problems as 'invalid sloc' when dumping 'printf': it
is the variadic argument (...) in the function declaration which
creates troubles."
  :type 'boolean)

;; This is our target file.c:
;; // file.c
;; /** This is a comment. */
;; extern int foobar(int a, const char *__restrict b);
;; /** This is another comment. */
;; int foobar(int a, char* b);
;; // file.c ends here
;;
;; Clang 3.5.0 produces the following AST for the file.c:
;; Dumping foobar_1:
;; FunctionDecl 0x384b460 <<stdin>:3:1, col:52> col:12 foobar_1 'int (int, const char *restrict)' extern
;; |-ParmVarDecl 0x384b2b0 <col:21, col:25> col:25 a 'int'
;; |-ParmVarDecl 0x384b350 <col:28, col:51> col:51 b 'const char *restrict'
;; `-FullComment 0x3888190 <line:2:4, col:23>
;;   `-ParagraphComment 0x3888160 <col:4, col:23>
;;       `-TextComment 0x3888130 <col:4, col:23> Text=" This is a comment. "
;;
;; Dumping foobar_2:
;; FunctionDecl 0x3888040 <<stdin>:5:1, col:28> col:5 foobar_2 'int (int, char *)'
;; |-ParmVarDecl 0x384b550 <col:14, col:18> col:18 a 'int'
;; |-ParmVarDecl 0x384b5f0 <col:21, col:27> col:27 b 'char *'
;; `-FullComment 0x3888260 <line:4:4, col:29>
;;   `-ParagraphComment 0x3888230 <col:4, col:29>
;;       `-TextComment 0x3888200 <col:4, col:29> Text=" This is another comment. "
;;
(defun company-clang--strip-meta (candidate)
  "Retrun CANDIDATE's meta stripped from prefix and args."
  (let* (stripped-meta
         (prefix (regexp-quote candidate))
         (meta (company-clang--meta candidate))
         (strip-prefix (format "\\(%s\\).*\\'" prefix))
         (strip-pointers "\*\\([a-zA-Z0-9_:]+\\)\\(?:,\\|)\\)")
         (strip-args "\\( [a-zA-Z0-9_:]+\\)\\(?:,\\|)\\)"))
    (when meta
      ;; Strip the prefix first.
      (setq stripped-meta
            (replace-regexp-in-string
             strip-prefix "" meta nil nil 1))
      ;; Always strip pointers before regular arguments, this will
      ;; disambiguate between a real pointer and a keyword.
      (setq stripped-meta
            (replace-regexp-in-string
             strip-pointers "" stripped-meta nil nil 1))
      ;; Finally, strip regular arguments.
      (setq stripped-meta
            (replace-regexp-in-string
             strip-args "" stripped-meta nil nil 1)))
    ;; If there is no meta, the candidate could be a type, so we use
    ;; the prefix as meta.
    (or stripped-meta prefix)))

(defun company-clang--parse-AST (candidate)
  "Return the CANDIDATE's AST.

Resolve function overloads by searching the candidate's meta in
the Clang's AST."
  (goto-char (point-min))
  (let* ((prefix (regexp-quote candidate))
         (meta (company-clang--strip-meta candidate))
         (type (string-match "^[^()]+$" meta))
         (head (format "^Dumping %s:$" prefix))
         (decl (format "^.* %s '\\(.*\\)'.*$" prefix))
         (abort nil)
         proto head-beg head-end empty-line)
    (while (not abort)
      (if (not (re-search-forward head nil t))
          (setq abort t)
        (setq head-beg (match-beginning 0))
        (setq head-end (match-end 0))
        (if (not (re-search-forward "^$" nil t))
            (setq abort t)
          (setq empty-line (match-end 0))
          (goto-char (+ head-end 1))
          (when (re-search-forward decl empty-line t)
            (setq proto (match-string-no-properties 1))
            ;; If `meta' is a type declaration, `proto' must be a type
            ;; declaration too.  Otherwise `proto' and `meta' should
            ;; match.
            (when (or (and type (string-match "^[^()]+$" proto))
                      (string= proto meta))
              (setq abort 'ok))))))
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

;; This is our target file.c:
;; // file.c
;; // Dump Clang's AST with:
;; // $ cat file.c | clang -fno-color-diagnostics -fsyntax-only -w -Xclang -ast-dump -Xclang -ast-dump-filter -Xclang foobar -x c -
;;
;; /**
;;  * Append @a postfix to @a string.
;;  *
;;  * @param[in,out]   string      A string object
;;  *                              to read and update.
;;  * @param[in]       postfix     The string postfix.
;;  *
;;  * @returns @c 1 if the function succeeded, @c 0 on error.
;;  */
;;
;; int foobar(char *string, char *postfix);
;; // file.c ends here
;;
;; Clang 3.5.0 produces the following AST for the file.c:
;; Dumping foobar:
;; FunctionDecl 0x3210420 <<stdin>:15:1, col:39> col:5 foobar 'int (char *, char *)'
;; |-ParmVarDecl 0x32102e0 <col:12, col:18> col:18 string 'char *'
;; |-ParmVarDecl 0x3210350 <col:26, col:32> col:32 postfix 'char *'
;; `-FullComment 0x324e4f0 <line:6:3, line:12:58>
;;   |-ParagraphComment 0x3210610 <line:6:3, col:26>
;;   | |-TextComment 0x3210510 <col:3, col:10> Text=" Append "
;;   | |-InlineCommandComment 0x3210560 <col:11, col:12> Name="a" RenderEmphasized Arg[0]="postfix"
;;   | |-TextComment 0x3210580 <col:21, col:24> Text=" to "
;;   | `-InlineCommandComment 0x32105d0 <col:25, col:26> Name="a" RenderEmphasized Arg[0]="string."
;;   |-ParagraphComment 0x3210660 <line:8:3>
;;   | `-TextComment 0x3210630 <col:3> Text=" "
;;   |-ParamCommandComment 0x3210680 <col:4, line:10:3> [in,out] explicitly Param="string" ParamIndex=0
;;   | `-ParagraphComment 0x324e220 <line:8:27, line:10:3>
;;   |   |-TextComment 0x324e1a0 <line:8:27, col:47> Text="      A string object"
;;   |   |-TextComment 0x324e1c0 <line:9:3, col:51> Text="                              to read and update."
;;   |   `-TextComment 0x324e1e0 <line:10:3> Text=" "
;;   |-ParamCommandComment 0x324e240 <col:4, col:51> [in] explicitly Param="postfix" ParamIndex=1
;;   | `-ParagraphComment 0x324e2d0 <col:28, col:51>
;;   |   `-TextComment 0x324e2a0 <col:28, col:51> Text="     The string postfix."
;;   |-ParagraphComment 0x324e320 <line:12:3>
;;   | `-TextComment 0x324e2f0 <col:3> Text=" "
;;   `-BlockCommandComment 0x324e340 <col:4, col:58> Name="returns"
;;     `-ParagraphComment 0x324e4a0 <col:12, col:58>
;;       |-TextComment 0x324e370 <col:12> Text=" "
;;       |-InlineCommandComment 0x324e3c0 <col:13, col:14> Name="c" RenderMonospaced Arg[0]="1"
;;       |-TextComment 0x324e3e0 <col:17, col:44> Text=" if the function succeeded, "
;;       |-InlineCommandComment 0x324e430 <col:45, col:46> Name="c" RenderMonospaced Arg[0]="0"
;;       `-TextComment 0x324e450 <col:49, col:58> Text=" on error."
;;
(defun company-clang--get-ast-doc (ast)
  "Get the AST's comments.

Return the AST's comments."
  (let (doc last-line line-begin line-end empty-lines)
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
                current-line comment next-pos key value)
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
                       "^<[^>]*> \\([^ ]+ \\).*Param=\"\\(.*\\)\".*$" value)
                  ;; Add a gap (spaces) of the same length of the
                  ;; string '@param' between the two captured groups.
                  (setq comment (concat (match-string 1 value)
                                        "      "
                                        (match-string 2 value)))))
               (t
                ;; We are dealing with an unknown `key', it is better
                ;; to write a stub.
                (when (string-match "^<[^>]*> \\(.*\\)$" value)
                  (setq comment
                        (concat "STUB[" (match-string 1 value) "]")))))
              (when search-comments
                ;; FIXME: we need to add missing spaces to the in-line
                ;; comments reading the 'col' values.
                ;;
                ;; Handles new-lines and in-line comments.
                (when (and current-line doc)
                  (when (or (not previous-line)
                            (> current-line previous-line))
                    (setq doc (concat doc "\n"))))
                (setq previous-line (or current-line previous-line))
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
          (list "-x" (when company-clang-parse-comments-as-c++-mode
                       (when (string= "c" (company-clang--lang-option)) "c++")))
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
