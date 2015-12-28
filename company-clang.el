;;; company-clang.el --- company-mode completion backend for Clang  -*- lexical-binding: t -*-

;; Copyright (C) 2009, 2011, 2013-2016  Free Software Foundation, Inc.

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
  "Completion backend for Clang."
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

;; The options "-ast-dump -ast-dump-filter" are part of Clang since version 3.2,
;; but can parse comments starting from version 3.3.
;;
;; NOTE: Clang can parse only comments wrote in Doxygen style.
(defconst company-clang-dump-ast-min-version 3.3
  "Minimum Clang's version to use for AST dumps.")

(defcustom company-clang-ast-system-headers-comments nil
  "Show system headers comments in the AST dump."
  :type 'boolean)

(defun company-clang--can-parse-comments nil
  "Return non-nil if the Clang's version in use is appropriate to parse
comments."
  (>= company-clang--version
      company-clang-dump-ast-min-version))

(defun company-clang--doc-buffer (candidate)
  "Create the documentation buffer for a CANDIDATE."
  ;; If there's no meta, maybe the CANDIDATE is a tag. In this case we use the
  ;; CANDIDATE's prefix as meta.
  (let ((meta (or (company-clang--meta candidate) (regexp-quote candidate)))
         (doc (company-clang--get-doc candidate)))
    (when doc
      (company-doc-buffer
       (concat meta "\n\n" doc)))))

(defun company-clang--get-doc (candidate)
  "Return the documentation found in the CANDIDATE's source file(s)."
  (let ((info-list (company-clang--describe-candidate candidate))
        doc item)
    (while info-list
      (when (setq item (company-clang--process-info (pop info-list)))
        (setq doc (concat doc (when doc "\n\n") item))))
    doc))

;; TODO:
;; - use the column number to locate the comment?
;; - parse comments on the same line of the declaration?
(defun company-clang--process-info (info)
  "Return the documentation found in the source file as expressed by the
candidate's INFO."
  (when (company-clang--can-parse-comments)
    (let* ((source-file (company-clang--value 'source-file info))
           (line-begin (company-clang--value 'line-begin info))
           (source-buffer (company-clang--source-buffer source-file))
           pos doc-begin doc-end doc)
      (when (and source-buffer line-begin)
        (with-current-buffer source-buffer
          (save-excursion
            ;; Line of the candidate.
            (goto-char (point-min))
            (forward-line (1- line-begin))
            ;; Search a comment block in the lines before.
            (or (setq pos (company-clang--doc-//)) (setq pos (company-clang--doc-/*))))
          (when pos
            (setq doc-begin (car pos))
            (setq doc-end (cdr pos))
            (company-clang--buffer-highlight source-buffer doc-begin doc-end)
            (setq doc (buffer-substring-no-properties doc-begin doc-end))
            (format "%s: %d\n%s"
                    (company-clang--source-file source-file) line-begin
                    (company-clang--trim-doc doc))))))))

(defun company-clang--source-file (file)
  "Evaluate the candidate's FILE name.

Return the candidate's source file."
  (when (stringp file)
    (if (string= file "<stdin>") (buffer-file-name)
      ;; Convert relative to absolute path.
      (expand-file-name file (file-name-directory (buffer-file-name))))))

(defun company-clang--source-buffer (file)
  "Evaluate the candidate's SOURCE-FILE.

Return the candidate's source buffer."
  (let (buf (source-file (company-clang--source-file file)))
    (when (and source-file (file-exists-p source-file))
      (setq buf (get-file-buffer source-file))
      ;; Use the opened candidate's buffer only if it's unmodified.
      (when (or (not buf) (buffer-modified-p buf))
        (setq buf (get-buffer-create "*clang-doc*"))
        (with-current-buffer buf
          (buffer-disable-undo)
          (erase-buffer)
          (insert-file-contents source-file)))
      buf)))

(defun company-clang--buffer-highlight (buffer begin end)
  "Highlight the text in the BUFFER between BEGIN and END.

Highlight the text only when the BUFFER is not a file buffer."
  (unless (buffer-file-name buffer)
    (put-text-property begin end 'face '(:foreground "red") buffer)))

(defun company-clang--doc-// nil
  "Search a block of // comment in the lines preceding the current line.

Return a cons of position begin and end of the comment."
  (let (pos-begin pos-end)
    (save-excursion
      ;; Previous line.
      (goto-char (1- (line-beginning-position)))
      ;; Skip previous empty lines.
      (while (re-search-backward "^$" (line-beginning-position) t)
        (goto-char (1- (line-beginning-position))))
      (while (re-search-backward "^[ \t]*//[^\n]*$" (line-beginning-position) t)
        (setq pos-begin (match-beginning 0))
        (when (not pos-end) (setq pos-end (match-end 0)))
        ;; Previous line.
        (goto-char (1- (line-beginning-position)))))
    (when (and pos-begin pos-end)
      (cons pos-begin pos-end))))

(defun company-clang--doc-/* nil
  "Search a block of /**/ comment in the lines preceding the current line.

Return a cons of position begin and end of the comment."
  (let (pos-begin pos-end)
    (save-excursion
      ;; Previous line.
      (goto-char (1- (line-beginning-position)))
      ;; Skip previous empty lines.
      (while (re-search-backward "^$" (line-beginning-position) t)
        (goto-char (1- (line-beginning-position))))
      (when (re-search-backward "[*]/" (line-beginning-position) t)
        (setq pos-end (match-end 0))
        (when (re-search-backward "^[ \t]*/[*]" nil t)
          (setq pos-begin (match-beginning 0)))))
    (when (and pos-begin pos-end)
      (cons pos-begin pos-end))))

(defun company-clang--trim-doc (doc)
  "Remove surrounding white spaces from the DOC, but keep the indentation."
  (let (left-spaces trimmed line)
    (when (stringp doc)
      (with-temp-buffer
        (insert doc)
        (goto-char (point-min))
        ;; First line.
        (when (re-search-forward "^\\([ \t]*\\)\\([^\n]*\\)[ \t]*$" nil t)
          (setq left-spaces (match-string 1))
          (setq trimmed (match-string 2))
          (goto-char (1+ (match-end 0)))
          ;; Lines after the first.
          (while (re-search-forward "^\\([^\n]*\\)[ \t]*$" nil t)
            (setq line (match-string 1))
            (goto-char (1+ (match-end 0)))
            ;; Remove LEFT-SPACES from the beginning of line.
            (when (string-match (format "^%s" left-spaces) line)
              (setq line (substring line (match-end 0))))
            (setq trimmed (concat trimmed "\n" line)))
          trimmed)))))

(defun company-clang--describe-candidate (candidate)
  "Describe the CANDIDATE within the current context.

Return the CANDIDATE's info-list."
  (let ((context (company-clang--get-context candidate)))
    (company-clang--process-context context)))

(defun company-clang--filter (action candidate)
  "Execute ACTION on the CANDIDATE's filter property.

If set, the filter property suggests to filter the CANDIDATE's AST dump.

ACTION:
 'get: return the filter property.
 'put: enable the filter property.
 'rem: remove the filter property."
  (cond
   ((eq action 'get)
    (get-text-property 0 'filter candidate))
   ((eq action 'put)
    (put-text-property 0 1 'filter t candidate))
   ((eq action 'rem)
    (remove-text-properties 0 1 (list 'filter) candidate))))

(defun company-clang--root (action candidate)
  "Execute ACTION on the CANDIDATE's root property.

If set, the root property suggests that the CANDIDATE doesn't have a parent
in the AST dump (it is not a member of a data structure).

ACTION:
 'get: return the root property.
 'put: enable the root property.
 'rem: remove the root property."
  (cond
   ((eq action 'get)
    (get-text-property 0 'root candidate))
   ((eq action 'put)
    (put-text-property 0 1 'root t candidate))
   ((eq action 'rem)
    (remove-text-properties 0 1 (list 'root) candidate))))

(defun company-clang--get-context (candidate)
  "Return a list of identifiers depicting the context of the CANDIDATE.

If the CANDIDATE is a member of a data structure, then the list represents
that branch, otherwise the list contains only the CANDIDATE.

The processing is performed in the current buffer. `point' is at the
location of any charachter of the CANDIDATE, or at its beginning/end."
  (let (parent context)
    (save-excursion
      ;; Gobble any CANDIDATE's prefix.
      (while (re-search-backward "[A-Za-z0-9_]" (- (point) 1) t))
      ;; Construct the context.
      (while (or (re-search-backward "[.]" (- (point) 1) t)
                 (re-search-backward "->" (- (point) 2) t)
                 (re-search-backward "::" (- (point) 2) t))
        (while (re-search-backward "[A-Za-z0-9_]" (- (point) 1) t)
          (setq parent (concat (match-string-no-properties 0) parent)))
        (when parent
          (push parent context)
          (setq parent nil))))
    ;; Set the CANDIDATE's filter property (this is sticky).
    (company-clang--filter 'put candidate)
    (unless context
      ;; Set the CANDIDATE's root property (this is sticky).
      (company-clang--root 'put candidate))
    (setq context (append context (list candidate)))
    context))

;; The Clang's completion list doesn't give sufficient hints to know if the
;; completion is about a typedef or a struct (or enum). So, we collect each
;; possible counterpart found in the AST dump.
(defun company-clang--process-context (context)
  "Unfold the CONTEXT and highlight each candidate's info in its AST dump buffer.

Return the candidate's info-list."
  (let (inner outer seek seek-resume candidate buf info-list)
    ;; Still more identifiers in CONTEXT?
    (while context
      (setq seek t)
      (setq outer inner)
      (setq candidate (pop context))
      (setq buf (company-clang--dump-AST candidate))
      ;; Collect the first candidate's info.
      (with-current-buffer buf
        ;; Filter the AST dump if required.
        (when (company-clang--filter 'get candidate)
          (company-clang--filter-AST candidate))
        (goto-char (point-min))
        (while (and seek (setq inner (company-clang--get-info candidate)) outer)
          (when (company-clang--nested-info inner outer)
            (setq seek nil)))
        (setq seek-resume (point))))
    (company-clang--highlight-info inner)
    (push inner info-list)
    ;; Collect the remaining candidate's info.
    (when inner
      (with-current-buffer buf
        (goto-char seek-resume)
        (while (setq inner (company-clang--get-info candidate))
          (when (or (not outer) (company-clang--nested-info inner outer))
            (company-clang--highlight-info inner)
            (push inner info-list)))))
    info-list))

(defconst company-clang--ast-error "clang-ast-error")

(defconst company-clang--ast-error-buffer
  (concat "*" company-clang--ast-error "*"))

(defconst company-clang--ast-error-file
  (concat temporary-file-directory company-clang--ast-error))

(defun company-clang--dump-AST (candidate)
  "Dump the CANDIDATE's AST in a buffer, then return that buffer.

This processing requires the CANDIDATE's buffer as current buffer."
  ;; NOTE: call `company-clang--build-AST-args' from the CANDIDATE's buffer.
  (let* (process
         (prefix (regexp-quote candidate))
         (args (company-clang--build-AST-args prefix))
         (buf (get-buffer-create "*clang-ast*"))
         ;; Looks unnecessary in Emacs 25.1 and later.
         (process-adaptive-read-buffering nil))
    (unless (get-buffer-process buf)
      (with-current-buffer buf
        (buffer-disable-undo)
        (erase-buffer))
      ;; NOTE: start the process from the CANDIDATE's buffer.
      (setq process (apply #'call-process-region (point-min) (point-max)
                           company-clang-executable
                           nil (list buf company-clang--ast-error-file) nil args))
      ;; Check for errors.
      (unless (eq 0 process)
        (company-clang--handle-ast-error process args))
      buf)))

(defun company-clang--handle-ast-error (res args)
  (with-current-buffer (get-buffer-create company-clang--ast-error-buffer)
    (let ((inhibit-read-only t)
          (cmd (concat company-clang-executable
                       " "
                       (mapconcat 'identity args " "))))
      (erase-buffer)
      (insert (current-time-string)
              (format "\nclang failed with error %d:\n" res)
              cmd "\n\n")
      (insert-file-contents company-clang--ast-error-file)
      (setq buffer-read-only t)
      (goto-char (point-min)))))

(defun company-clang--filter-AST (candidate)
  "Remove non-CANDIDATE results from the AST dump buffer.

The processing starts from the beginning of the current buffer.

Return non-nil if the current buffer has been modified."
  (goto-char (point-min))
  (let (modified ast-begin ast-end)
    ;; Identify AST section.
    (while (re-search-forward "^Dumping [^\n]+:$" nil t)
      (setq ast-begin (match-beginning 0))
      (goto-char (1+ (match-end 0)))
      ;; Empty line at the end.
      (when (re-search-forward "^$" nil t)
        (setq ast-end (match-end 0))
        (unless (company-clang--match-candidate candidate ast-begin ast-end)
          (setq modified t)
          (delete-region ast-begin (1+ ast-end)))))
    modified))

;; TODO:
;; - is it necessary to replace all .* with [^\n]* to stay on the same line?
;; - are there cases when we want to cross lines?
(defun company-clang--match-candidate (candidate ast-begin ast-end)
  "Return a cons of position begin and end of the first matching CANDIDATE
found between AST-BEGIN and AST-END of the AST dump buffer.

The processing starts from AST-BEGIN of the current buffer.

Handle function overloads and 'invalid sloc' results.

The 'invalid sloc' dilemma is caused by the presence of the variadic
argument (...) in a declaration (see printf). Only Clang's c-mode is
affected (-x c), c++-mode (-x c++) seems to be immune."
  (goto-char ast-begin)
  (let* ((prefix (regexp-quote candidate))
         (meta (company-clang--meta candidate))
         (head (format "^Dumping \\(?:\\([^\n]*\\)::\\)?%s:$" prefix))
         (decl
          (format
           "^\\(.*Decl\\) .* %s '\\([^'(\n]*\\)\\([^']*)\\( [*]\\)?\\)?'.*$"
           prefix))
         (enum (format "^\\(EnumDecl\\) [^\n]* %s$" prefix))
         (strc (format "^\\(RecordDecl\\) [^\n]* struct %s definition$" prefix))
         (unio (format "^\\(RecordDecl\\) [^\n]* union %s definition$" prefix))
         (root (company-clang--root 'get candidate))
         match-found
         pos-begin pos-end pos-resume
         parent obj proto args ptr variadic AST-meta type name)
    (when (re-search-forward head ast-end t)
      (setq parent (match-string-no-properties 1))
      (setq pos-begin (match-beginning 0))
      (goto-char (1+ (match-end 0)))
      (setq pos-resume (point))
      (when (re-search-forward "^$" ast-end t)
        (setq pos-end (match-end 0))
        (goto-char pos-resume)
        (when (or (re-search-forward decl pos-end t)
                  (re-search-forward enum pos-end t)
                  (re-search-forward strc pos-end t)
                  (re-search-forward unio pos-end t))
          (goto-char (1+ (match-end 0)))
          (setq obj (match-string-no-properties 1))
          (setq proto (match-string-no-properties 2))
          (setq args (match-string-no-properties 3))
          (setq ptr (match-string-no-properties 4))
          (setq variadic (if args (string-match "^(.*[.][.][.])$" args) nil))
          (when (if root (or (not parent) (string= obj "EnumConstantDecl")) t)
            ;; If META is "::", the candidate is a tag not belonging to the
            ;; current data structure (i.e. an enum tag which is used in the
            ;; context of a struct).
            (cond
             ;; If META is nil, the candidate is a tag and it could be a typedef,
             ;; enum, or struct or (union). In the case of the typedef, PROTO is
             ;; the name of the type. In the case of an enum, struct (or union)
             ;; PROTO is nil.
             ((and (not meta)
                   (or (string= obj "TypedefDecl")
                       (string= obj "EnumDecl")
                       (string= obj "RecordDecl")))
              (setq match-found t))
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
              (setq match-found t))
             ;; Anonymous declaration, usually an enum or a struct.
             ((and args
                   (string-match "(anonymous [^ \n]+ at.*)" args)
                   (string= meta
                            (concat proto "(anonymous)" (when ptr ptr) " " prefix)))
              (setq match-found t))
             ;; When ARGS is nil, and the candidate is not a typedef, enum, or
             ;; struct (or union) tag, then the candidate is a variable when META
             ;; is equal to the variable's declaration. In this case, PROTO is the
             ;; name of the type.
             ((and (not args) (string= meta (concat proto " " prefix)))
              (setq match-found t))
             ((not (null proto))
              ;; Reconstruct the function declaration from the AST.
              (setq AST-meta nil)
              (while (re-search-forward
                      "^.*ParmVarDecl.* \\([^>\n]*\\) '\\([^'\n]*\\)'.*$" pos-end t)
                (setq type (match-string-no-properties 2))
                (setq name (match-string-no-properties 1))
                (setq AST-meta
                      (concat AST-meta (if AST-meta ", ")
                              type (unless (string= (substring type -1) "*") " ")
                              name)))
              (setq AST-meta (concat proto (when (string= (substring proto -1) "*") " ")
                                     prefix "(" AST-meta (if variadic ", ...") ")"))
              (when (string= meta AST-meta)
                (setq match-found t))))))))
    (when match-found
      (cons pos-begin pos-end))))

(defun company-clang--get-info (candidate)
  "Collect info about the CANDIDATE from the AST dump buffer.

The processing starts from `point' in the current buffer."
  (let ((prefix (regexp-quote candidate))
        pos-resume ast-begin ast-end line column source-file info)
    (when (re-search-forward (format "^Dumping \\(?:[^\n]*::\\)?%s:$" prefix) nil t)
      (setq ast-begin (match-beginning 0))
      (goto-char (1+ (match-end 0)))
      (setq pos-resume (point))
      (when (re-search-forward "^$" nil t)
        (setq ast-end (match-end 0))
        (goto-char pos-resume)
        ;; Greedy regexp on the declaration line.
        (when (re-search-forward ".*> " (line-end-position) t)
          ;; AST section.
          (push (cons 'ast-end ast-end) info)
          (push (cons 'ast-begin ast-begin) info)
          ;; AST dump buffer name.
          (push (cons 'ast-buffer (buffer-name)) info)
          ;; Declaration line:column end.
          (or (re-search-backward
               "line:\\([0-9]+\\):\\([0-9]+\\)" (line-beginning-position) t)
              (re-search-backward
               "\\(\\)col:\\([0-9]+\\)" (line-beginning-position) t))
          (setq line (company-clang--sum (match-string-no-properties 1)))
          (setq column (company-clang--sum (match-string-no-properties 2) -1))
          (push (cons 'column-end column) info)
          (push (cons 'line-end line) info)
          ;; Declaration line:column begin.
          (re-search-backward ":\\([0-9]+\\):\\([0-9]+\\)" (line-beginning-position) t)
          (setq line (company-clang--sum (match-string-no-properties 1)))
          (setq column (company-clang--sum (match-string-no-properties 2) -1))
          (push (cons 'column-begin column) info)
          (push (cons 'line-begin line) info)
          ;; CANDIDATE's source file (repeat the non-greedy regexp).
          (while (re-search-backward "<\\(.+\\)" (line-beginning-position) t)
            (setq source-file (concat (match-string-no-properties 1) source-file))
            (goto-char (1+ (point))))
          (push (cons 'source-file source-file) info)
          (goto-char (1+ ast-end))
          info)))))

(defun company-clang--sum (&rest strings-or-numbers)
  "Return sum of STRINGS-OR-NUMBERS.

Return nil if only one argument is neither a string nor a number, or if an
argument is an empty string."
  (let ((sum 0)
        (num (pop strings-or-numbers)))
    (when strings-or-numbers
      (setq sum (apply 'company-clang--sum strings-or-numbers)))
    (when (and (stringp num) (not (string= num "")))
      (setq num (string-to-number num)))
    (when (and (numberp num) (numberp sum))
      (+ num sum))))

(defun company-clang--value (field info)
  "Extract the value of FIELD from a candidate's INFO."
  (cdr (assoc field info)))

(defun company-clang--highlight-info (info)
  "Highlight the candidate's INFO in its AST dump buffer."
  (let ((buf (company-clang--value 'ast-buffer info))
        (ast-begin (company-clang--value 'ast-begin info))
        (ast-end (company-clang--value 'ast-end info)))
    (when (and buf ast-begin ast-end)
      (with-current-buffer (get-buffer buf)
        (put-text-property ast-begin ast-end 'face '(:foreground "red"))))))

(defun company-clang--nested-info (inner outer)
  "Return non-nil if INNER is inside OUTER, otherwise return nil."
  (let ((outer-begin (company-clang--value 'line-begin outer))
        (outer-end (company-clang--value 'line-end outer))
        (inner-begin (company-clang--value 'line-begin inner))
        (inner-end (company-clang--value 'line-end inner)))
    (when (and inner-begin outer-begin outer-end)
      (when (and (> inner-begin outer-begin) (< inner-begin outer-end))
        (when (or (not inner-end) (< inner-end outer-end)) t)))))

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
  (let ((ann (company-clang--annotation-1 candidate)))
    (if (not (and ann (string-prefix-p "(*)" ann)))
        ann
      (with-temp-buffer
        (insert ann)
        (search-backward ")")
        (let ((pt (1+ (point))))
          (re-search-forward ".\\_>" nil t)
          (delete-region pt (point)))
        (buffer-string)))))

(defun company-clang--annotation-1 (candidate)
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

(defun company-clang--start-process (prefix callback &rest args)
  (let ((objc (derived-mode-p 'objc-mode))
        (buf (get-buffer-create "*clang-output*"))
        ;; Looks unnecessary in Emacs 25.1 and later.
        (process-adaptive-read-buffering nil))
    (if (get-buffer-process buf)
        (funcall callback nil)
      (with-current-buffer buf
        (erase-buffer)
        (setq buffer-undo-list t))
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
  "Return Clang's AST dump args for the current buffer.

Use PREFIX as AST dump filter."
  (append '("-fno-color-diagnostics" "-fsyntax-only" "-w"
            "-Xclang" "-ast-dump" "-Xclang" "-ast-dump-filter"
            "-Xclang")
          (list prefix)
          (when company-clang-ast-system-headers-comments
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

(defun company-clang (command &optional arg &rest ignored)
  "`company-mode' completion backend for Clang.
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
                             (company-template-objc-templatify anno)
                           (company-template-c-like-templatify
                            (concat arg anno))))))))

(provide 'company-clang)
;;; company-clang.el ends here
