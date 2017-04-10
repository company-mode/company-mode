;;; template-tests.el --- company-mode tests  -*- lexical-binding: t -*-

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
(require 'company-template)

(defun company-template-field-assert-text (str &optional pos)
  (let ((field (company-template-field-at pos)))
    (should (equal (buffer-substring-no-properties
                    (overlay-start field)
                    (overlay-end field))
                   str))))

(ert-deftest company-template-removed-after-the-last-jump ()
  (with-temp-buffer
    (insert "{ }")
    (goto-char 2)
    (let ((tpl (company-template-declare-template (point) (1- (point-max)))))
      (save-excursion
        (dotimes (_ 2)
          (insert " foo")
          (company-template-add-field tpl (- (point) 3) (point))))
      (company-call 'template-forward-field)
      (should (= 3 (point)))
      (company-call 'template-forward-field)
      (should (= 7 (point)))
      (company-call 'template-forward-field)
      (should (= 11 (point)))
      (should (zerop (length (overlay-get tpl 'company-template-fields))))
      (should (null (overlay-buffer tpl))))))

(ert-deftest company-template-removed-after-input-and-jump ()
  (with-temp-buffer
    (insert "{ }")
    (goto-char 2)
    (let ((tpl (company-template-declare-template (point) (1- (point-max)))))
      (save-excursion
        (insert " bar")
        (company-template-add-field tpl (- (point) 3) (point)))
      (company-call 'template-move-to-first tpl)
      (should (= 3 (point)))
      (dolist (c (string-to-list "tee"))
        (let ((last-command-event c))
          (company-call 'self-insert-command 1)))
      (should (string= "{ tee }" (buffer-string)))
      (should (overlay-buffer tpl))
      (company-call 'template-forward-field)
      (should (= 7 (point)))
      (should (null (overlay-buffer tpl))))))

(ert-deftest company-template-c-like-templatify ()
  (with-temp-buffer
    (let ((text "foo(int a, short b)"))
      (insert text)
      (company-template-c-like-templatify text)
      (should (equal "foo(int a, short b)" (buffer-string)))
      (company-template-field-assert-text "int a"))))

(ert-deftest company-template-c-like-templatify-trims-after-closing-paren ()
  (with-temp-buffer
    (let ((text "foo(int a, short b)!@ #1334 a"))
      (insert text)
      (company-template-c-like-templatify text)
      (should (equal "foo(int a, short b)" (buffer-string)))
      (company-template-field-assert-text "int a"))))

(ert-deftest company-template-c-like-templatify-generics ()
  (with-temp-buffer
    (let ((text "foo<TKey, TValue>(int i, Dict<TKey, TValue>, long l)"))
      (insert text)
      (company-template-c-like-templatify text)
      (should (equal (buffer-string) text))
      (company-template-field-assert-text "TKey")
      (search-forward "Dict")
      (forward-char -1)
      (company-template-field-assert-text "Dict<TKey, TValue>"))))

(ert-deftest company-template-c-like-func-ptr ()
  (with-temp-buffer
    (let ((text "foo(*)(int)"))
      (insert text)
      (company-template-c-like-templatify text)
      (should (equal (buffer-string) "foo(int)"))
      (company-template-field-assert-text "int"))))

(ert-deftest company-clang-objc-templatify-empty-args ()
  (with-temp-buffer
    (let ((text "createBookWithTitle:andAuthor:"))
      (insert text)
      (company-template-objc-templatify text)
      (should (equal "createBookWithTitle:arg0 andAuthor:arg1" (buffer-string)))
      (should (looking-at "arg0"))
      (should (null (overlay-get (company-template-field-at) 'display))))))

(ert-deftest company-template-objc-templatify ()
  (with-temp-buffer
    (let ((text "createBookWithTitle:(NSString) andAuthor:(id)"))
      (insert text)
      (company-template-objc-templatify text)
      (should (equal (buffer-string) text))
      (company-template-field-assert-text "(NSString)"))))

(ert-deftest company-template-clear-field-c-like-first-arg ()
  (with-temp-buffer
    (let ((text "foo(int a, short b)"))
      (insert text)
      (company-template-c-like-templatify text)
      (company-template-clear-field)
      (should (equal "foo(short b)" (buffer-string))))))

(ert-deftest company-template-clear-field-c-like-last-arg ()
  (with-temp-buffer
    (let ((text "foo(int a, short b)"))
      (insert text)
      (company-template-c-like-templatify text)
      (insert "42")
      (company-template-forward-field)
      (company-template-clear-field)
      (should (equal "foo(42)" (buffer-string))))))

(ert-deftest company-template-clear-field-c-like-generic-1 ()
  (with-temp-buffer
    (let ((text "foo<TValue>(int a, short b)"))
      (insert text)
      (company-template-c-like-templatify text)
      (company-template-clear-field)
      (should (equal "foo<>(int a, short b)" (buffer-string))))))

(ert-deftest company-template-clear-field-c-like-generic-2 ()
  (with-temp-buffer
    (let ((text "foo<TKey, TValue>(int a, short b)"))
      (insert text)
      (company-template-c-like-templatify text)
      (company-template-clear-field)
      (should (equal "foo<TValue>(int a, short b)" (buffer-string))))))

(ert-deftest company-template-clear-field-objc-first-arg ()
  (with-temp-buffer
    (let ((text "createBookWithTitle:andAuthor:"))
      (insert text)
      (company-template-objc-templatify text)
      (company-template-clear-field)
      (should (equal "createBookWithTitle: andAuthor:arg1" (buffer-string))))))

(ert-deftest company-template-insert-hook-c-like-field ()
  (with-temp-buffer
    (let ((text "foo(int a, short b)"))
      (insert text)
      (company-template-c-like-templatify text)
      (let ((ovl (company-template-field-at (point))))
        (company-template-insert-hook ovl nil)
        (should (equal "foo(, short b)" (buffer-string)))))))
