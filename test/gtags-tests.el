;;; gtags-tests.el --- company-mode tests  -*- lexical-binding: t -*-

;; Copyright (C) 2021  Free Software Foundation, Inc.

;; Author: Aymeric Agon-Rambosson

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
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

(require 'company-tests)
(require 'company-gtags)

(ert-deftest company-gtags-cpp-macro-variable ()
  (let ((str (propertize
              "PGT_ADDRESS_MASK"
              'meta "#define PGT_ADDRESS_MASK 0xFFFFFFFFFF000")))
    (should (null (company-gtags 'annotation str)))))

(ert-deftest company-gtags-cpp-macro-function ()
  (let ((str (propertize
              "ADDRESS_FROM_STACK"
              'meta "#define ADDRESS_FROM_STACK(a) ((TEXT_BASE > a) && (a >= STACK_BASE))")))
    (should (equal (company-gtags 'annotation str)
                   "(a)"))))

(ert-deftest company-gtags-C-function ()
  (let ((str (propertize
              "munmap"
              'meta "void munmap(struct task *ctx, vaddr_t vaddr)")))
    (should (equal (company-gtags 'annotation str)
                   "(struct task *ctx, vaddr_t vaddr)"))))

(ert-deftest company-gtags-C-type ()
  (let ((str (propertize
              "uint64_t"
              'meta "typedef unsigned long int   uint64_t;")))
    (should (null (company-gtags 'annotation str)))))

(ert-deftest company-gtags-C-struct ()
  (let ((str (propertize
              "interrupt_context"
              'meta "struct interrupt_context")))
    (should (null (company-gtags 'annotation str)))))

(ert-deftest company-gtags-C-function-with-lots-of-parens-in-arg ()
  (let ((str (propertize
              "test"
              'meta "static void test(void (*foo)(void (*bar)(void)))")))
    (should (equal (company-gtags 'annotation str)
                   "(void (*foo)(void (*bar)(void)))"))))

(ert-deftest company-gtags-C-function-multiline-args ()
  (let ((str (propertize
              "munmap"
              'meta "void munmap(")))
    (should (equal (company-gtags 'annotation str) "(" ))))
