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

(ert-deftest company-clang-func-ptr-annotation ()
  (let ((str (propertize "foo" 'meta "void (*)(int) foo")))
    (should (equal (company-clang 'annotation str) "(*)(int)"))))

(ert-deftest company-clang-null-annotation ()
  (let ((str "char"))
    (should (null (company-clang 'annotation str)))))
