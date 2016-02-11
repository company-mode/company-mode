;;; async-tests.el --- company-mode tests  -*- lexical-binding: t -*-

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

(defun company-async-backend (command &optional _)
  (pcase command
    (`prefix "foo")
    (`candidates
     (cons :async
           (lambda (cb)
             (run-with-timer 0.05 nil
                             #'funcall cb '("abc" "abd")))))))

(ert-deftest company-call-backend-forces-sync ()
  (let ((company-backend 'company-async-backend)
        (company-async-timeout 0.1))
    (should (equal '("abc" "abd") (company-call-backend 'candidates)))))

(ert-deftest company-call-backend-errors-on-timeout ()
  (with-temp-buffer
    (let* ((company-backend (lambda (command &optional _arg)
                              (pcase command
                                (`candidates (cons :async 'ignore)))))
           (company-async-timeout 0.1)
           (err (should-error (company-call-backend 'candidates "foo"))))
      (should (string-match-p "async timeout" (cadr err))))))

(ert-deftest company-call-backend-raw-passes-return-value-verbatim ()
  (let ((company-backend 'company-async-backend))
    (should (equal "foo" (company-call-backend-raw 'prefix)))
    (should (equal :async (car (company-call-backend-raw 'candidates "foo"))))
    (should (equal 'closure (cadr (company-call-backend-raw 'candidates "foo"))))))

(ert-deftest company-manual-begin-forces-async-candidates-to-sync ()
  (with-temp-buffer
    (company-mode)
    (let (company-frontends
          company-transformers
          (company-backends (list 'company-async-backend)))
      (company-manual-begin)
      (should (equal "foo" company-prefix))
      (should (equal '("abc" "abd") company-candidates)))))

(ert-deftest company-idle-begin-allows-async-candidates ()
  (with-temp-buffer
    (company-mode)
    (let (company-frontends
          company-transformers
          (company-backends (list 'company-async-backend)))
      (company-idle-begin (current-buffer) (selected-window)
                          (buffer-chars-modified-tick) (point))
      (should (null company-candidates))
      (sleep-for 0.1)
      (should (equal "foo" company-prefix))
      (should (equal '("abc" "abd") company-candidates)))))

(ert-deftest company-idle-begin-cancels-async-candidates-if-buffer-changed ()
  (with-temp-buffer
    (company-mode)
    (let (company-frontends
          (company-backends (list 'company-async-backend)))
      (company-idle-begin (current-buffer) (selected-window)
                          (buffer-chars-modified-tick) (point))
      (should (null company-candidates))
      (insert "a")
      (sleep-for 0.1)
      (should (null company-candidates))
      (should (null company-candidates-cache))
      (should (null company-backend)))))

(ert-deftest company-idle-begin-async-allows-immediate-callbacks ()
  (with-temp-buffer
    (company-mode)
    (let (company-frontends
          (company-backends
           (list (lambda (command &optional arg)
                   (pcase command
                     (`prefix (buffer-substring (point-min) (point)))
                     (`candidates
                      (let ((c (all-completions arg '("abc" "def"))))
                        (cons :async
                              (lambda (cb) (funcall cb c)))))
                     (`no-cache t)))))
          (company-minimum-prefix-length 0))
      (company-idle-begin (current-buffer) (selected-window)
                          (buffer-chars-modified-tick) (point))
      (should (equal '("abc" "def") company-candidates))
      (let ((last-command-event ?a))
        (company-call 'self-insert-command 1))
      (should (equal '("abc") company-candidates)))))

(ert-deftest company-multi-backend-forces-prefix-to-sync ()
  (with-temp-buffer
    (let ((company-backend (list 'ignore
                                 (lambda (command)
                                   (should (eq command 'prefix))
                                   (cons :async
                                         (lambda (cb)
                                           (run-with-timer
                                            0.01 nil
                                            (lambda () (funcall cb nil))))))
                                 (lambda (command)
                                   (should (eq command 'prefix))
                                   "foo"))))
      (should (equal "foo" (company-call-backend-raw 'prefix))))
    (let ((company-backend (list (lambda (_command)
                                   (cons :async
                                         (lambda (cb)
                                           (run-with-timer
                                            0.01 nil
                                            (lambda () (funcall cb "bar"))))))
                                 (lambda (_command)
                                   "foo"))))
      (should (equal "bar" (company-call-backend-raw 'prefix))))))

(ert-deftest company-multi-backend-merges-deferred-candidates ()
  (with-temp-buffer
    (let* ((immediate (lambda (command &optional _)
                        (pcase command
                          (`prefix "foo")
                          (`candidates
                           (cons :async
                                 (lambda (cb) (funcall cb '("f"))))))))
           (company-backend (list 'ignore
                                  (lambda (command &optional arg)
                                    (pcase command
                                      (`prefix "foo")
                                      (`candidates
                                       (should (equal arg "foo"))
                                       (cons :async
                                             (lambda (cb)
                                               (run-with-timer
                                                0.01 nil
                                                (lambda () (funcall cb '("a" "b")))))))))
                                  (lambda (command &optional _)
                                    (pcase command
                                      (`prefix "foo")
                                      (`candidates '("c" "d" "e"))))
                                  immediate)))
      (should (equal :async (car (company-call-backend-raw 'candidates "foo"))))
      (should (equal '("a" "b" "c" "d" "e" "f")
                     (company-call-backend 'candidates "foo")))
      (let ((company-backend (list immediate)))
        (should (equal '("f") (company-call-backend 'candidates "foo")))))))

(ert-deftest company-multi-backend-merges-deferred-candidates-2 ()
  (with-temp-buffer
    (let ((company-backend (list (lambda (command &optional _)
                                   (pcase command
                                     (`prefix "foo")
                                     (`candidates
                                      (cons :async
                                            (lambda (cb) (funcall cb '("a" "b")))))))
                                 (lambda (command &optional _)
                                   (pcase command
                                     (`prefix "foo")
                                     (`candidates
                                      (cons :async
                                            (lambda (cb) (funcall cb '("c" "d")))))))
                                 (lambda (command &optional _)
                                   (pcase command
                                     (`prefix "foo")
                                     (`candidates
                                      (cons :async
                                            (lambda (cb) (funcall cb '("e" "f"))))))))))
      (should (equal :async (car (company-call-backend-raw 'candidates "foo"))))
      (should (equal '("a" "b" "c" "d" "e" "f")
                     (company-call-backend 'candidates "foo"))))))

(ert-deftest company-multi-backend-merges-deferred-candidates-3 ()
  (with-temp-buffer
    (let ((company-backend (list (lambda (command &optional _)
                                   (pcase command
                                     (`prefix "foo")
                                     (`candidates
                                      (cons :async
                                            (lambda (cb) (funcall cb '("a" "b")))))))
                                 (lambda (command &optional _)
                                   (pcase command
                                     (`prefix "foo")
                                     (`candidates
                                      (cons :async
                                            (lambda (cb)
                                              (run-with-timer
                                               0.01 nil
                                               (lambda ()
                                                 (funcall cb '("c" "d")))))))))
                                 (lambda (command &optional _)
                                   (pcase command
                                     (`prefix "foo")
                                     (`candidates
                                      (cons :async
                                            (lambda (cb)
                                              (run-with-timer
                                               0.01 nil
                                               (lambda ()
                                                 (funcall cb '("e" "f"))))))))))))
      (should (equal :async (car (company-call-backend-raw 'candidates "foo"))))
      (should (equal '("a" "b" "c" "d" "e" "f")
                     (company-call-backend 'candidates "foo"))))))
