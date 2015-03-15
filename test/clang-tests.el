;;; clang-tests.el --- company-mode tests  -*- lexical-binding: t -*-

;; Copyright (C) 2015  Free Software Foundation, Inc.

;; Author: Dmitry Gutov

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

(require 'company-tests)
(require 'company-clang)

(ert-deftest company-clang-objc-templatify ()
  (with-temp-buffer
    (let ((text "createBookWithTitle:andAuthor:"))
      (insert text)
      (company-clang-objc-templatify text)
      (should (equal "createBookWithTitle:arg0 andAuthor:arg1" (buffer-string)))
      (should (looking-at "arg0"))
      (should (null (overlay-get (company-template-field-at) 'display))))))

(ert-deftest company-clang-simple-annotation ()
  (let ((str (propertize
              "foo" 'meta
              "wchar_t * wmemchr(wchar_t *__p, wchar_t __c, size_t __n)")))
    (should (equal (company-clang 'annotation str)
                   "(wchar_t *__p, wchar_t __c, size_t __n)"))))

(ert-deftest company-clang-generic-annotation ()
  (let ((str (propertize
              "foo" 'meta
              "shared_ptr<_Tp> make_shared<typename _Tp>(_Args &&__args...)")))
    (should (equal (company-clang 'annotation str)
                   "<typename _Tp>(_Args &&__args...)"))))

(defvar company-clang-parse-doc-max-wait 30
  "Maximum waiting time before aborting when `company-clang-parse-doc-busy'
  is non-nil")

(defvar company-clang-parse-doc-busy nil
  "Prevent race conditions when parsing documentation.")

;; Validated with Clang version 3.5.0.
;; Steps:
;; 1. write test file
;; 2. build candidates list
;; 3. dump AST with Clang
;; 4. parse AST for each candidate
;; 5. find documentation "OK"
(ert-deftest company-clang-full-parse-c-mode ()
  ;; FIXME: test disabled due to unexpected travis build system failures.
  (when company-clang-executable
    (let* ((tmp-file (concat temporary-file-directory "test.c"))
           (buf (get-file-buffer tmp-file))
           (total-wait 0)
           (wait 0.5)
           abort)
      (while (and (not abort) company-clang-parse-doc-busy)
        (if (>= total-wait company-clang-parse-doc-max-wait)
            (setq abort t)
          (setq total-wait (+ total-wait wait))
          (sleep-for wait)))
      (unless abort
        (setq company-clang-parse-doc-busy t)
        (when buf
          (set-buffer buf)
          (set-buffer-modified-p nil)
          (kill-buffer))
        (write-region "// test.c
// anonymous enum
enum {
  /**OK1*/
  TEST_FIRST,
  /**OK2*/
  TEST_SECOND
}
  /**OK3*/
  test_enum_a;

enum test_my_enum {
  /**OK4*/
  TEST_ONE,
  /**OK5*/
  TEST_TWO
}
  /**OK6*/
  test_enum_b;

// anonymous struct
struct {
  int alfa;
}
  /**OK7*/
  test_struct_a;

struct test_my_struct {
  int beta;
}
  /**OK8*/
  test_struct_b;

/**OK9*/
typedef unsigned char test_char_t;

/**OK10*/
extern int printf (const char *__restrict __format, ...);

/**OK11*/
int test_function_a(char *message, int ref, char *output, int pos);

// use c++ reserved keywords as names
struct class {
  int gamma;
}
  /**OK12*/
  template;

void test() {

}" nil tmp-file)
        (setq buf (find-file-noselect tmp-file t))
        (switch-to-buffer buf)
        (company-mode)
        (goto-char (- (point-max) 2))
        (company-clang--candidates
         ""
         (lambda (candidates-list)
           (let* ((tmp-file (concat temporary-file-directory "test.c"))
                  (buf (find-file-noselect tmp-file t))
                  (prefixes-list '("TEST_FIRST"      ;; anonymous enum's member
                                   "TEST_SECOND"     ;; anonymous enum's member
                                   "test_enum_a"     ;; anonymous enum
                                   "TEST_ONE"        ;; enum's member
                                   "TEST_TWO"        ;; enum's member
                                   "test_enum_b"     ;; enum
                                   "test_struct_a"   ;; anonymous struct
                                   "test_struct_b"   ;; struct
                                   "test_char_t"     ;; type
                                   "printf"          ;; variadic and 'invalid sloc'
                                   "test_function_a" ;; various args and pointers
                                   "template"        ;; c++ keyword as name
                                   ))
                  prefixes-used prefix)
             (dolist (candidate candidates-list)
               (setq prefix (regexp-quote candidate))
               (when (member prefix prefixes-list)
                 (sleep-for 0.1)
                 (switch-to-buffer buf)
                 (setq prefixes-used (append prefixes-used (list prefix)))
                 (should (string-match-p
                          "\\`OK[0-9]+"
                          (company-clang--get-candidate-doc candidate)))))
             (should (equal (sort prefixes-list #'string<)
                            (sort prefixes-used #'string<)))
             (setq company-clang-parse-doc-busy nil))))))))

;; Validated with Clang version 3.5.0.
;; Steps:
;; 1. write test file
;; 2. build candidates list
;; 3. dump AST with Clang
;; 4. parse AST for each candidate
;; 5. find documentation "OK"
(ert-deftest company-clang-full-parse-c++-mode ()
  ;; FIXME: test disabled due to unexpected travis build system failures.
  (when company-clang-executable
    (let* ((tmp-file (concat temporary-file-directory "test.cpp"))
           (buf (get-file-buffer tmp-file))
           (total-wait 0)
           (wait 0.5)
           abort)
      (while (and (not abort) company-clang-parse-doc-busy)
        (if (>= total-wait company-clang-parse-doc-max-wait)
            (setq abort t)
          (setq total-wait (+ total-wait wait))
          (sleep-for wait)))
      (unless abort
        (setq company-clang-parse-doc-busy t)
        (when buf
          (set-buffer buf)
          (set-buffer-modified-p nil)
          (kill-buffer))
        (write-region "// test.cpp
// anonymous enum
enum {
  /**OK1*/
  TEST_FIRST,
  /**OK2*/
  TEST_SECOND
}
  /**OK3*/
  test_enum_a;

enum test_my_enum {
  /**OK4*/
  TEST_ONE,
  /**OK5*/
  TEST_TWO
}
  /**OK6*/
  test_enum_b;

// anonymous struct
struct {
  int alfa;
}
  /**OK7*/
  test_struct_a;

struct test_my_struct {
  int beta;
}
  /**OK8*/
  test_struct_b;

/**OK9*/
typedef unsigned char test_char_t;

/**OK10*/
extern int printf (const char *__restrict __format, ...);

/**OK11*/
int test_function_a(char *message, int ref, char *output, int pos);

/**OK12*/
int test_function_b(char *message, int *ref, char *output, int pos);

void test() {

}" nil tmp-file)
        (setq buf (find-file-noselect tmp-file t))
        (switch-to-buffer buf)
        (company-mode)
        (goto-char (- (point-max) 2))
        (company-clang--candidates
         ""
         (lambda (candidates-list)
           (let* ((tmp-file (concat temporary-file-directory "test.cpp"))
                  (buf (find-file-noselect tmp-file t))
                  (prefixes-list '("TEST_FIRST"      ;; anonymous enum's member
                                   "TEST_SECOND"     ;; anonymous enum's member
                                   "test_enum_a"     ;; anonymous enum
                                   "TEST_ONE"        ;; enum's member
                                   "TEST_TWO"        ;; enum's member
                                   "test_enum_b"     ;; enum
                                   "test_struct_a"   ;; anonymous struct
                                   "test_struct_b"   ;; struct
                                   "test_char_t"     ;; type
                                   "printf"          ;; variadic and 'invalid sloc'
                                   "test_function_a" ;; various args and pointers
                                   "test_function_b" ;; various args and pointers
                                   ))
                  prefixes-used prefix)
             (dolist (candidate candidates-list)
               (setq prefix (regexp-quote candidate))
               (when (member prefix prefixes-list)
                 (sleep-for 0.1)
                 (switch-to-buffer buf)
                 (setq prefixes-used (append prefixes-used (list prefix)))
                 (should (string-match-p
                          "\\`OK[0-9]+"
                          (company-clang--get-candidate-doc candidate)))))
             (should (equal (sort prefixes-list #'string<)
                            (sort prefixes-used #'string<)))
             (setq company-clang-parse-doc-busy nil))))))))

;; Validated with Clang version 3.5.0.
;; Steps:
;; 1. write test file
;; 2. build candidates list
;; 3. dump AST with Clang
;; 4. parse AST for each candidate
;; 5. verify interpretation of documentation
(ert-deftest company-clang-verify-doc-c-mode ()
  (when company-clang-executable
    (let* ((tmp-file (concat temporary-file-directory "test.c"))
           (buf (get-file-buffer tmp-file))
           (total-wait 0)
           (wait 0.5)
           abort)
      (while (and (not abort) company-clang-parse-doc-busy)
        (if (>= total-wait company-clang-parse-doc-max-wait)
            (setq abort t)
          (setq total-wait (+ total-wait wait))
          (sleep-for wait)))
      (unless abort
        (setq company-clang-parse-doc-busy t)
        (when buf
          (set-buffer buf)
          (set-buffer-modified-p nil)
          (kill-buffer))
        (write-region "// test.c
/**
 * Append @a postfix to @a string.
 *
 * @param[in,out]   string      A string object
 *                              to read and update.
 * @param[in]       postfix     The string postfix.
 *
 * @returns @c 1 if the function succeeded, @c 0 on error.
 */

int foobar(char *string, char *postfix);

void test() {

}" nil tmp-file)
        (setq buf (find-file-noselect tmp-file t))
        (switch-to-buffer buf)
        (company-mode)
        (goto-char (- (point-max) 2))
        (company-clang--candidates
         ""
         (lambda (candidates-list)
           (let* ((tmp-file (concat temporary-file-directory "test.c"))
                  (buf (find-file-noselect tmp-file t))
                  (prefixes-list '("foobar"))
                  prefixes-used prefix)
             (dolist (candidate candidates-list)
               (setq prefix (regexp-quote candidate))
               (when (member prefix prefixes-list)
                 (sleep-for 0.1)
                 (switch-to-buffer buf)
                 (setq prefixes-used (append prefixes-used (list prefix)))
                 (should (string=
                          (company-clang--get-candidate-doc candidate)
                          " Append postfix to string.

 [in,out] string              A string object
                              to read and update.
 [in]     postfix             The string postfix.

 returns
 1 if the function succeeded, 0 on error."))))
             (should (equal (sort prefixes-list #'string<)
                            (sort prefixes-used #'string<)))
             (setq company-clang-parse-doc-busy nil))))))))

;; Validated with Clang version 3.5.0.
;; Steps:
;; 1. write test file
;; 2. build candidates list
;; 3. dump AST with Clang
;; 4. parse AST for each candidate
;; 5. verify interpretation of documentation
(ert-deftest company-clang-verify-doc-c++-mode ()
  (when company-clang-executable
    (let* ((tmp-file (concat temporary-file-directory "test.cpp"))
           (buf (get-file-buffer tmp-file))
           (total-wait 0)
           (wait 0.5)
           abort)
      (while (and (not abort) company-clang-parse-doc-busy)
        (if (>= total-wait company-clang-parse-doc-max-wait)
            (setq abort t)
          (setq total-wait (+ total-wait wait))
          (sleep-for wait)))
      (unless abort
        (setq company-clang-parse-doc-busy t)
        (when buf
          (set-buffer buf)
          (set-buffer-modified-p nil)
          (kill-buffer))
        (write-region "// test.cpp
/**
 * Append @a postfix to @a string.
 *
 * @param[in,out]   string      A string object
 *                              to read and update.
 * @param[in]       postfix     The string postfix.
 *
 * @returns @c 1 if the function succeeded, @c 0 on error.
 */

int foobar(char *string, char *postfix);

void test() {

}" nil tmp-file)
        (setq buf (find-file-noselect tmp-file t))
        (switch-to-buffer buf)
        (company-mode)
        (goto-char (- (point-max) 2))
        (company-clang--candidates
         ""
         (lambda (candidates-list)
           (let* ((tmp-file (concat temporary-file-directory "test.cpp"))
                  (buf (find-file-noselect tmp-file t))
                  (prefixes-list '("foobar"))
                  prefixes-used prefix)
             (dolist (candidate candidates-list)
               (setq prefix (regexp-quote candidate))
               (when (member prefix prefixes-list)
                 (sleep-for 0.1)
                 (switch-to-buffer buf)
                 (setq prefixes-used (append prefixes-used (list prefix)))
                 (should (string=
                          (company-clang--get-candidate-doc candidate)
                          " Append postfix to string.

 [in,out] string              A string object
                              to read and update.
 [in]     postfix             The string postfix.

 returns
 1 if the function succeeded, 0 on error."))))
             (should (equal (sort prefixes-list #'string<)
                            (sort prefixes-used #'string<)))
             (setq company-clang-parse-doc-busy nil))))))))

;; Validated with Clang version 3.5.0.
;; This test should be able to run without the need of Clang. Also it doesn't
;; require "race condition" management.
;; Steps:
;; 1. write AST expected matches
;; 2. build candidates list from completions
;; 3. verify AST matches
(ert-deftest company-clang-AST-parse-c-mode ()
  (let ((ast-results '(("TEST_FIRST"
                        "Dumping ::TEST_FIRST:
EnumConstantDecl 0x1bde580 <<stdin>:5:3> col:3 TEST_FIRST 'int'
`-FullComment 0x1c1e300 <line:4:6, col:7>
  `-ParagraphComment 0x1c1e2d0 <col:6, col:7>
    `-TextComment 0x1c1e2a0 <col:6, col:7> Text=\"OK\"
")
                       ("TEST_SECOND"
                        "Dumping ::TEST_SECOND:
EnumConstantDecl 0x2e42600 <<stdin>:7:3> col:3 TEST_SECOND 'int'
`-FullComment 0x2e81310 <line:6:6, col:7>
  `-ParagraphComment 0x2e812e0 <col:6, col:7>
    `-TextComment 0x2e812b0 <col:6, col:7> Text=\"OK\"
")
                       ("test_enum_a"
                        "Dumping test_enum_a:
VarDecl 0x209e6d0 <<stdin>:3:1, line:10:3> col:3 test_enum_a 'enum (anonymous enum at <stdin>:3:1)':'enum (anonymous at <stdin>:3:1)'
`-FullComment 0x20de300 <line:9:6, col:7>
  `-ParagraphComment 0x20de2d0 <col:6, col:7>
    `-TextComment 0x20de2a0 <col:6, col:7> Text=\"OK\"
")
                       ("TEST_ONE"
                        "Dumping test_my_enum::TEST_ONE:
EnumConstantDecl 0x27df820 <<stdin>:14:3> col:3 referenced TEST_ONE 'int'
`-FullComment 0x281e310 <line:13:6, col:7>
  `-ParagraphComment 0x281e2e0 <col:6, col:7>
    `-TextComment 0x281e2b0 <col:6, col:7> Text=\"OK\"
")
                       ("TEST_TWO"
                        "Dumping test_my_enum::TEST_TWO:
EnumConstantDecl 0x265f230 <<stdin>:16:3> col:3 referenced TEST_TWO 'int'
`-FullComment 0x2661310 <line:15:6, col:7>
  `-ParagraphComment 0x26612e0 <col:6, col:7>
    `-TextComment 0x26612b0 <col:6, col:7> Text=\"OK\"
")
                       ("test_enum_b"
                        "Dumping test_enum_b:
VarDecl 0x31c5300 <<stdin>:12:1, line:19:3> col:3 test_enum_b 'enum test_my_enum':'enum test_my_enum'
`-FullComment 0x31c7350 <line:18:6, col:7>
  `-ParagraphComment 0x31c7320 <col:6, col:7>
    `-TextComment 0x31c72f0 <col:6, col:7> Text=\"OK\"
")
                       ("test_struct_a"
                        "Dumping test_struct_a:
VarDecl 0x21734f0 <<stdin>:22:1, line:26:3> col:3 test_struct_a 'struct (anonymous struct at <stdin>:22:1)':'struct (anonymous at <stdin>:22:1)'
`-FullComment 0x2176300 <line:25:6, col:7>
  `-ParagraphComment 0x21762d0 <col:6, col:7>
    `-TextComment 0x21762a0 <col:6, col:7> Text=\"OK\"
")
                       ("test_struct_b"
                        "Dumping test_struct_b:
VarDecl 0x1cde6e0 <<stdin>:28:1, line:32:3> col:3 test_struct_b 'struct test_my_struct':'struct test_my_struct'
`-FullComment 0x1ce1300 <line:31:6, col:7>
  `-ParagraphComment 0x1ce12d0 <col:6, col:7>
    `-TextComment 0x1ce12a0 <col:6, col:7> Text=\"OK\"
")
                       ("test_char_t"
                        "Dumping test_char_t:
TypedefDecl 0x36f7790 <<stdin>:35:1, col:23> col:23 test_char_t 'unsigned char'
`-FullComment 0x36fa300 <line:34:4, col:5>
  `-ParagraphComment 0x36fa2d0 <col:4, col:5>
    `-TextComment 0x36fa2a0 <col:4, col:5> Text=\"OK\"
")
                       ("printf"
                        "Dumping printf:
FunctionDecl 0x1c5b950 <<stdin>:38:12> col:12 implicit referenced printf 'int (const char *, ...)' extern
|-ParmVarDecl 0x1c5b9f0 <<invalid sloc>> <invalid sloc> 'const char *'
`-FormatAttr 0x1c5ba50 <col:12> Implicit printf 1 2
"
                        "Dumping printf:
FunctionDecl 0x1c5baa0 prev 0x1c5b950 <<stdin>:38:1, col:56> col:12 used printf 'int (const char *, ...)' extern
|-ParmVarDecl 0x1c5b850 <col:20, col:43> col:43 __format 'const char *restrict'
|-FormatAttr 0x1c5bb70 <col:12> Inherited printf 1 2
`-FullComment 0x1c5d350 <line:37:4, col:5>
  `-ParagraphComment 0x1c5d320 <col:4, col:5>
    `-TextComment 0x1c5d2f0 <col:4, col:5> Text=\"OK\"
")
                       ("test_function_a"
                        "Dumping test_function_a:
FunctionDecl 0x1f79e50 <<stdin>:41:1, col:66> col:5 test_function_a 'int (char *, int, char *, int)'
|-ParmVarDecl 0x1f79c10 <col:21, col:27> col:27 message 'char *'
|-ParmVarDecl 0x1f79c80 <col:36, col:40> col:40 ref 'int'
|-ParmVarDecl 0x1f79cf0 <col:45, col:51> col:51 output 'char *'
|-ParmVarDecl 0x1f79d60 <col:59, col:63> col:63 pos 'int'
`-FullComment 0x1f7c300 <line:40:4, col:5>
  `-ParagraphComment 0x1f7c2d0 <col:4, col:5>
    `-TextComment 0x1f7c2a0 <col:4, col:5> Text=\"OK\"
")
                       ("template"
                        "Dumping template:
VarDecl 0x2703090 <<stdin>:44:1, line:48:3> col:3 used template 'struct class':'struct class'
`-FullComment 0x2704330 <line:47:6, col:7>
  `-ParagraphComment 0x2704300 <col:6, col:7>
    `-TextComment 0x27042d0 <col:6, col:7> Text=\"OK\"
")))
        (clang-output "COMPLETION: TEST_FIRST : [#enum <anonymous>#]TEST_FIRST
COMPLETION: TEST_SECOND : [#enum <anonymous>#]TEST_SECOND
COMPLETION: test_enum_a : [#enum (anonymous)#]test_enum_a
COMPLETION: TEST_ONE : [#enum test_my_enum#]TEST_ONE
COMPLETION: TEST_TWO : [#enum test_my_enum#]TEST_TWO
COMPLETION: test_enum_b : [#enum test_my_enum#]test_enum_b
COMPLETION: test_struct_a : [#struct (anonymous)#]test_struct_a
COMPLETION: test_struct_b : [#struct test_my_struct#]test_struct_b
COMPLETION: test_char_t : test_char_t
COMPLETION: printf : [#int#]printf(<#const char *restrict __format, ...#>)
COMPLETION: test_function_a : [#int#]test_function_a(<#char *message#>, <#int ref#>, <#char *output#>, <#int pos#>)
COMPLETION: template : [#struct class#]template")
        clang-ast candidates-list prefix ast-expected ast-match)
    (dolist (record ast-results)
      (dolist (item (cdr record))
        (setq clang-ast (concat clang-ast (when clang-ast "\n") item))))
    (with-temp-buffer
      (insert clang-output)
      (setq candidates-list (company-clang--parse-output "" nil)))
    (with-temp-buffer
      (insert clang-ast)
      (dolist (candidate candidates-list)
        (setq prefix (regexp-quote candidate))
        (setq ast-expected (car (last (assoc prefix ast-results))))
        (setq ast-match (company-clang--parse-AST candidate))
        (should (string= ast-match ast-expected))))))

;; Validated with Clang version 3.5.0.
;; This test should be able to run without the need of Clang. Also it doesn't
;; require "race condition" management.
;; Steps:
;; 1. write AST expected matches
;; 2. build candidates list from completions
;; 3. verify AST matches
(ert-deftest company-clang-AST-parse-c++-mode ()
  (let ((ast-results '(("TEST_FIRST"
                        "Dumping ::TEST_FIRST:
EnumConstantDecl 0x1f6f990 <<stdin>:5:3> col:3 TEST_FIRST 'enum (anonymous at <stdin>:3:1)'
`-FullComment 0x1fb3450 <line:4:6, col:7>
  `-ParagraphComment 0x1fb3420 <col:6, col:7>
    `-TextComment 0x1fb33f0 <col:6, col:7> Text=\"OK\"
")
                       ("TEST_SECOND"
                        "Dumping ::TEST_SECOND:
EnumConstantDecl 0x225fa10 <<stdin>:7:3> col:3 TEST_SECOND 'enum (anonymous at <stdin>:3:1)'
`-FullComment 0x22a3480 <line:6:6, col:7>
  `-ParagraphComment 0x22a3450 <col:6, col:7>
    `-TextComment 0x22a3420 <col:6, col:7> Text=\"OK\"
")
                       ("test_enum_a"
                        "Dumping test_enum_a:
VarDecl 0x1c5fae0 <<stdin>:3:1, line:10:3> col:3 test_enum_a 'enum (anonymous enum at <stdin>:3:1)':'enum (anonymous at <stdin>:3:1)'
`-FullComment 0x1ca3450 <line:9:6, col:7>
  `-ParagraphComment 0x1ca3420 <col:6, col:7>
    `-TextComment 0x1ca33f0 <col:6, col:7> Text=\"OK\"
")
                       ("TEST_ONE"
                        "Dumping test_my_enum::TEST_ONE:
EnumConstantDecl 0x2cbd6d0 <<stdin>:14:3> col:3 TEST_ONE 'enum test_my_enum'
`-FullComment 0x2cc4450 <line:13:6, col:7>
  `-ParagraphComment 0x2cc4420 <col:6, col:7>
    `-TextComment 0x2cc43f0 <col:6, col:7> Text=\"OK\"
")
                       ("TEST_TWO"
                        "Dumping test_my_enum::TEST_TWO:
EnumConstantDecl 0x2109750 <<stdin>:16:3> col:3 referenced TEST_TWO 'enum test_my_enum'
`-FullComment 0x2110480 <line:15:6, col:7>
  `-ParagraphComment 0x2110450 <col:6, col:7>
    `-TextComment 0x2110420 <col:6, col:7> Text=\"OK\"
")
                       ("Dumping test_enum_b:
VarDecl 0x2b41820 <<stdin>:12:1, line:19:3> col:3 test_enum_b 'enum test_my_enum':'enum test_my_enum'
`-FullComment 0x2b48450 <line:18:6, col:7>
  `-ParagraphComment 0x2b48420 <col:6, col:7>
    `-TextComment 0x2b483f0 <col:6, col:7> Text=\"OK\"
")
                       ("test_struct_a"
                        "Dumping test_struct_a:
VarDecl 0x2494a80 <<stdin>:22:1, line:26:3> col:3 test_struct_a 'struct (anonymous struct at <stdin>:22:1)':'struct (anonymous at <stdin>:22:1)' callinit
|-CXXConstructExpr 0x2494e18 <col:3> 'struct (anonymous struct at <stdin>:22:1)':'struct (anonymous at <stdin>:22:1)' 'void (void)'
`-FullComment 0x249b450 <line:25:6, col:7>
  `-ParagraphComment 0x249b420 <col:6, col:7>
    `-TextComment 0x249b3f0 <col:6, col:7> Text=\"OK\"
")
                       ("test_struct_b"
                        "Dumping test_struct_b:
VarDecl 0x24030e0 <<stdin>:28:1, line:32:3> col:3 test_struct_b 'struct test_my_struct':'struct test_my_struct' callinit
|-CXXConstructExpr 0x2403448 <col:3> 'struct test_my_struct':'struct test_my_struct' 'void (void)'
`-FullComment 0x2409450 <line:31:6, col:7>
  `-ParagraphComment 0x2409420 <col:6, col:7>
    `-TextComment 0x24093f0 <col:6, col:7> Text=\"OK\"
")
                       ("test_char_t"
                        "Dumping test_char_t:
TypedefDecl 0x1f604d0 <<stdin>:35:1, col:23> col:23 test_char_t 'unsigned char'
`-FullComment 0x1f66450 <line:34:4, col:5>
  `-ParagraphComment 0x1f66420 <col:4, col:5>
    `-TextComment 0x1f663f0 <col:4, col:5> Text=\"OK\"
")
                       ("printf"
                        "Dumping printf:
FunctionDecl 0x2bd5ae0 <<stdin>:38:1, col:56> col:12 printf 'int (const char *restrict, ...)' extern
|-ParmVarDecl 0x2bd0570 <col:20, col:43> col:43 __format 'const char *restrict'
`-FullComment 0x2bd6430 <line:37:4, col:5>
  `-ParagraphComment 0x2bd6400 <col:4, col:5>
    `-TextComment 0x2bd63d0 <col:4, col:5> Text=\"OK\"
")
                       ("test_function_a"
                        "Dumping test_function_a:
FunctionDecl 0x2c80e60 <<stdin>:41:1, col:66> col:5 test_function_a 'int (char *, int, char *, int)'
|-ParmVarDecl 0x2c80c20 <col:21, col:27> col:27 message 'char *'
|-ParmVarDecl 0x2c80c90 <col:36, col:40> col:40 ref 'int'
|-ParmVarDecl 0x2c80d00 <col:45, col:51> col:51 output 'char *'
|-ParmVarDecl 0x2c80d70 <col:59, col:63> col:63 pos 'int'
`-FullComment 0x2c81450 <line:40:4, col:5>
  `-ParagraphComment 0x2c81420 <col:4, col:5>
    `-TextComment 0x2c813f0 <col:4, col:5> Text=\"OK\"
")
                       ("test_function_b"
                        "Dumping test_function_b:
FunctionDecl 0x30071d0 <<stdin>:44:1, col:67> col:5 test_function_b 'int (char *, int *, char *, int)'
|-ParmVarDecl 0x3006f60 <col:21, col:27> col:27 message 'char *'
|-ParmVarDecl 0x3007000 <col:36, col:41> col:41 ref 'int *'
|-ParmVarDecl 0x3007070 <col:46, col:52> col:52 output 'char *'
|-ParmVarDecl 0x30070e0 <col:60, col:64> col:64 pos 'int'
`-FullComment 0x3007450 <line:43:4, col:5>
  `-ParagraphComment 0x3007420 <col:4, col:5>
    `-TextComment 0x30073f0 <col:4, col:5> Text=\"OK\"
")))
        (clang-output "COMPLETION: TEST_FIRST : [#enum <anonymous>#]TEST_FIRST
COMPLETION: TEST_SECOND : [#enum <anonymous>#]TEST_SECOND
COMPLETION: test_enum_a : [#enum (anonymous)#]test_enum_a
COMPLETION: TEST_ONE : [#test_my_enum#]TEST_ONE
COMPLETION: TEST_TWO : [#test_my_enum#]TEST_TWO
COMPLETION: test_enum_b : [#enum test_my_enum#]test_enum_b
COMPLETION: test_struct_a : [#struct (anonymous)#]test_struct_a
COMPLETION: test_struct_b : [#struct test_my_struct#]test_struct_b
COMPLETION: test_char_t : test_char_t
COMPLETION: printf : [#int#]printf(<#const char *restrict __format, ...#>)
COMPLETION: test_function_a : [#int#]test_function_a(<#char *message#>, <#int ref#>, <#char *output#>, <#int pos#>)
COMPLETION: test_function_b : [#int#]test_function_b(<#char *message#>, <#int *ref#>, <#char *output#>, <#int pos#>)")
        clang-ast candidates-list prefix ast-expected ast-match)
    (dolist (record ast-results)
      (dolist (item (cdr record))
        (setq clang-ast (concat clang-ast (when clang-ast "\n") item))))
    (with-temp-buffer
      (insert clang-output)
      (setq candidates-list (company-clang--parse-output "" nil)))
    (with-temp-buffer
      (insert clang-ast)
      (dolist (candidate candidates-list)
        (setq prefix (regexp-quote candidate))
        (setq ast-expected (car (last (assoc prefix ast-results))))
        (setq ast-match (company-clang--parse-AST candidate))
        (should (string= ast-match ast-expected))))))

;; Validated with Clang version 3.5.0.
;; This test should be able to run without the need of Clang. Also it doesn't
;; require "race condition" management.
;; Steps:
;; 1. write AST expected matches
;; 2. write AST expected doc
;; 3. build candidates list from completions
;; 4. verify AST matches
;; 5. verify AST docs
(ert-deftest company-clang-AST-doc-c-mode ()
  (let ((ast-results '(("foobar"
                        "Dumping foobar:
FunctionDecl 0x350b420 <<stdin>:12:1, col:39> col:5 foobar 'int (char *, char *)'
|-ParmVarDecl 0x350b2e0 <col:12, col:18> col:18 string 'char *'
|-ParmVarDecl 0x350b350 <col:26, col:32> col:32 postfix 'char *'
`-FullComment 0x3549550 <line:3:3, line:9:58>
  |-ParagraphComment 0x3549130 <line:3:3, col:26>
  | |-TextComment 0x350b620 <col:3, col:10> Text=\" Append \"
  | |-InlineCommandComment 0x350b670 <col:11, col:12> Name=\"a\" RenderEmphasized Arg[0]=\"postfix\"
  | |-TextComment 0x350b690 <col:21, col:24> Text=\" to \"
  | `-InlineCommandComment 0x35490f0 <col:25, col:26> Name=\"a\" RenderEmphasized Arg[0]=\"string.\"
  |-ParagraphComment 0x3549180 <line:5:3>
  | `-TextComment 0x3549150 <col:3> Text=\" \"
  |-ParamCommandComment 0x35491a0 <col:4, line:7:3> [in,out] explicitly Param=\"string\" ParamIndex=0
  | `-ParagraphComment 0x3549280 <line:5:27, line:7:3>
  |   |-TextComment 0x3549200 <line:5:27, col:47> Text=\"      A string object\"
  |   |-TextComment 0x3549220 <line:6:3, col:51> Text=\"                              to read and update.\"
  |   `-TextComment 0x3549240 <line:7:3> Text=\" \"
  |-ParamCommandComment 0x35492a0 <col:4, col:51> [in] explicitly Param=\"postfix\" ParamIndex=1
  | `-ParagraphComment 0x3549330 <col:28, col:51>
  |   `-TextComment 0x3549300 <col:28, col:51> Text=\"     The string postfix.\"
  |-ParagraphComment 0x3549380 <line:9:3>
  | `-TextComment 0x3549350 <col:3> Text=\" \"
  `-BlockCommandComment 0x35493a0 <col:4, col:58> Name=\"returns\"
    `-ParagraphComment 0x3549500 <col:12, col:58>
      |-TextComment 0x35493d0 <col:12> Text=\" \"
      |-InlineCommandComment 0x3549420 <col:13, col:14> Name=\"c\" RenderMonospaced Arg[0]=\"1\"
      |-TextComment 0x3549440 <col:17, col:44> Text=\" if the function succeeded, \"
      |-InlineCommandComment 0x3549490 <col:45, col:46> Name=\"c\" RenderMonospaced Arg[0]=\"0\"
      `-TextComment 0x35494b0 <col:49, col:58> Text=\" on error.\"
")))
        (doc-results '(("foobar"
                        " Append postfix to string.

 [in,out] string              A string object
                              to read and update.
 [in]     postfix             The string postfix.

 returns
 1 if the function succeeded, 0 on error.")))
        (clang-output "COMPLETION: foobar : [#int#]foobar(<#char *string#>, <#char *postfix#>)")
        clang-ast candidates-list prefix
        ast-expected ast-match doc-expected doc-match)
    (dolist (record ast-results)
      (dolist (item (cdr record))
        (setq clang-ast (concat clang-ast (when clang-ast "\n") item))))
    (with-temp-buffer
      (insert clang-output)
      (setq candidates-list (company-clang--parse-output "" nil)))
    (with-temp-buffer
      (insert clang-ast)
      (dolist (candidate candidates-list)
        (setq prefix (regexp-quote candidate))
        (setq ast-match (company-clang--parse-AST candidate))
        (setq doc-match (company-clang--get-ast-doc ast-match))
        (setq ast-expected (car (last (assoc prefix ast-results))))
        (setq doc-expected (car (last (assoc prefix doc-results))))
        (should (string= ast-match ast-expected))
        (should (string= doc-match doc-expected))))))

;; TODO:
;; (ert-deftest company-clang-AST-doc-c++-mode ())
