;;; company-elisp-tests.el --- company-elisp tests

;; Copyright (C) 2013-2014  Free Software Foundation, Inc.

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


;;; Commentary:
;;

;;; Code:

(require 'company-elisp)

(defmacro company-elisp-with-buffer (contents &rest body)
  (declare (indent 0))
  `(with-temp-buffer
     (insert ,contents)
     (setq major-mode 'emacs-lisp-mode)
     (re-search-backward "|")
     (replace-match "")
     (let ((company-elisp-detect-function-context t))
       ,@body)))

(ert-deftest company-elisp-candidates-predicate ()
  (company-elisp-with-buffer
    "(foo ba|)"
    (should (eq (company-elisp--candidates-predicate "ba")
                'boundp))
    (should (eq (let (company-elisp-detect-function-context)
                  (company-elisp--candidates-predicate "ba"))
                'company-elisp--predicate)))
  (company-elisp-with-buffer
    "(foo| )"
    (should (eq (company-elisp--candidates-predicate "foo")
                'fboundp))
    (should (eq (let (company-elisp-detect-function-context)
                  (company-elisp--candidates-predicate "foo"))
                'company-elisp--predicate)))
  (company-elisp-with-buffer
    "(foo 'b|)"
    (should (eq (company-elisp--candidates-predicate "b")
                'company-elisp--predicate))))

(ert-deftest company-elisp-candidates-predicate-in-docstring ()
  (company-elisp-with-buffer
   "(def foo () \"Doo be doo `ide|"
   (should (eq 'company-elisp--predicate
               (company-elisp--candidates-predicate "ide")))))

;; This one's also an integration test.
(ert-deftest company-elisp-candidates-recognizes-binding-form ()
  (let ((company-elisp-detect-function-context t)
        (obarray [when what whelp])
        (what 1)
        (whelp 2)
        (wisp 3))
    (company-elisp-with-buffer
      "(let ((foo 7) (wh| )))"
      (should (equal '("what" "whelp")
                     (company-elisp-candidates "wh"))))
    (company-elisp-with-buffer
      "(cond ((null nil) (wh| )))"
      (should (equal '("when")
                     (company-elisp-candidates "wh"))))))

(ert-deftest company-elisp-candidates-predicate-binding-without-value ()
  (cl-loop for (text prefix predicate) in '(("(let (foo|" "foo" boundp)
                                            ("(let (foo (bar|" "bar" boundp)
                                            ("(let (foo) (bar|" "bar" fboundp))
           do
           (eval `(company-elisp-with-buffer
                   ,text
                   (should (eq ',predicate
                               (company-elisp--candidates-predicate ,prefix)))))))

(ert-deftest company-elisp-finds-vars ()
  (let ((obarray [boo bar baz backquote])
        (boo t)
        (bar t)
        (baz t))
    (should (equal '("bar" "baz")
                   (company-elisp--globals "ba" 'boundp)))))

(ert-deftest company-elisp-finds-functions ()
  (let ((obarray [when what whelp])
        (what t)
        (whelp t))
    (should (equal '("when")
                   (company-elisp--globals "wh" 'fboundp)))))

(ert-deftest company-elisp-finds-things ()
  (let ((obarray [when what whelp])
        (what t)
        (whelp t))
    (should (equal '("what" "whelp" "when")
                   (sort (company-elisp--globals "wh" 'company-elisp--predicate)
                         'string<)))))

(ert-deftest company-elisp-locals-vars ()
  (company-elisp-with-buffer
    "(let ((foo 5) (bar 6))
       (cl-labels ((borg ()))
         (lambda (boo baz)
           b|)))"
    (should (equal '("bar" "baz" "boo")
                   (company-elisp--locals "b" nil)))))

(ert-deftest company-elisp-locals-single-var ()
  (company-elisp-with-buffer
    "(dotimes (itk 100)
       (dolist (item items)
         it|))"
    (should (equal '("itk" "item")
                   (company-elisp--locals "it" nil)))))

(ert-deftest company-elisp-locals-funs ()
  (company-elisp-with-buffer
    "(cl-labels ((foo ())
                 (fee ()))
       (let ((fun 4))
         (f| )))"
    (should (equal '("fee" "foo")
                   (sort (company-elisp--locals "f" t) 'string<)))))

(ert-deftest company-elisp-locals-skips-current-varlist ()
  (company-elisp-with-buffer
    "(let ((foo 1)
           (f| )))"
    (should (null (company-elisp--locals "f" nil)))))

(ert-deftest company-elisp-show-locals-first ()
  (company-elisp-with-buffer
    "(let ((floo 1)
           (flop 2)
           (flee 3))
       fl|)"
    (let ((obarray [float-pi]))
      (let (company-elisp-show-locals-first)
        (should (eq nil (company-elisp 'sorted))))
      (let ((company-elisp-show-locals-first t))
        (should (eq t (company-elisp 'sorted)))
        (should (equal '("flee" "floo" "flop" "float-pi")
                       (company-elisp-candidates "fl")))))))

(ert-deftest company-elisp-candidates-no-duplicates ()
  (company-elisp-with-buffer
    "(let ((float-pi 4))
       f|)"
    (let ((obarray [float-pi])
          (company-elisp-show-locals-first t))
      (should (equal '("float-pi") (company-elisp-candidates "f"))))))

(ert-deftest company-elisp-shouldnt-complete-defun-name ()
  (company-elisp-with-buffer
    "(defun foob|)"
    (should (null (company-elisp 'prefix)))))

(ert-deftest company-elisp-should-complete-def-call ()
  (company-elisp-with-buffer
    "(defu|"
    (should (equal "defu" (company-elisp 'prefix)))))

(ert-deftest company-elisp-should-complete-in-defvar ()
  ;; It will also complete the var name, at least for now.
  (company-elisp-with-buffer
    "(defvar abc de|"
    (should (equal "de" (company-elisp 'prefix)))))

(ert-deftest company-elisp-shouldnt-complete-in-defun-arglist ()
  (company-elisp-with-buffer
    "(defsubst foobar (ba|"
    (should (null (company-elisp 'prefix)))))

(ert-deftest company-elisp-prefix-in-defun-body ()
  (company-elisp-with-buffer
    "(defun foob ()|)"
    (should (equal "" (company-elisp 'prefix)))))
