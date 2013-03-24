;;; company-tests.el --- company-mode tests

;; Copyright (C) 2011, 2013  Free Software Foundation, Inc.

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

(require 'ert)
(require 'company)
(require 'company-keywords)

(ert-deftest company-sorted-keywords ()
  "Test that keywords in `company-keywords-alist' are in alphabetical order."
  (dolist (pair company-keywords-alist)
    (when (consp (cdr pair))
      (let ((prev (cadr pair)))
        (dolist (next (cddr pair))
          (should (not (equal prev next)))
          (should (string< prev next))
          (setq prev next))))))

(ert-deftest company-good-prefix ()
  (let ((company-minimum-prefix-length 5)
        company--explicit-action)
    (should (eq t (company--good-prefix-p "!@#$%")))
    (should (eq nil (company--good-prefix-p "abcd")))
    (should (eq nil (company--good-prefix-p 'stop)))
    (should (eq t (company--good-prefix-p '("foo" . 5))))
    (should (eq nil (company--good-prefix-p '("foo" . 4))))))

(ert-deftest company-multi-backend-with-lambdas ()
  (let ((company-backend
         (list (lambda (command &optional arg &rest ignore)
                 (case command
                   (prefix "z")
                   (candidates '("a" "b"))))
               (lambda (command &optional arg &rest ignore)
                 (case command
                   (prefix "z")
                   (candidates '("c" "d")))))))
    (should (equal (company-call-backend 'candidates "z") '("a" "b" "c" "d")))))

(ert-deftest company-begin-backend-failure-doesnt-break-company-backends ()
  (with-temp-buffer
    (insert "a")
    (company-mode)
    (should-error
     (company-begin-backend (lambda (command &rest ignore))))
    (let (company-frontends
          (company-backends
           (list (lambda (command &optional arg)
                   (case command
                     (prefix "a")
                     (candidates '("a" "ab" "ac")))))))
      (let (this-command)
        (company-complete))
      (company-post-command)
      (should (eq 3 company-candidates-length)))))

(ert-deftest company-require-match-explicit ()
  (with-temp-buffer
    (insert "ab")
    (company-mode)
    (let (company-frontends
          (company-require-match 'company-explicit-action-p)
          (company-backends
           (list (lambda (command &optional arg)
                   (case command
                     (prefix (buffer-substring (point-min) (point)))
                     (candidates '("abc" "abd")))))))
      (let (this-command)
        (company-complete))
      (let ((last-command-event ?e))
        (self-insert-command 1))
      (company-post-command)
      (should (eq 2 company-candidates-length))
      (should (eq 3 (point))))))

(ert-deftest company-dont-require-match-when-idle ()
  (with-temp-buffer
    (insert "ab")
    (company-mode)
    (let (company-frontends
          (company-require-match 'company-explicit-action-p)
          (company-backends
           (list (lambda (command &optional arg)
                   (case command
                     (prefix (buffer-substring (point-min) (point)))
                     (candidates '("abc" "abd")))))))
      (company-idle-begin (current-buffer) (selected-window)
                          (buffer-chars-modified-tick) (point))
      (let ((last-command-event ?e))
        (self-insert-command 1))
      (company-post-command)
      (should (eq nil company-candidates-length))
      (should (eq 4 (point))))))

(ert-deftest company-auto-complete-explicit ()
  (with-temp-buffer
    (insert "ab")
    (company-mode)
    (let (company-frontends
          (company-auto-complete 'company-explicit-action-p)
          (company-auto-complete-chars '(? ))
          (company-backends
           (list (lambda (command &optional arg)
                   (case command
                     (prefix (buffer-substring (point-min) (point)))
                     (candidates '("abcd" "abef")))))))
      (let (this-command)
        (company-complete))
      (let ((last-command-event ? ))
        (self-insert-command 1))
      (company-post-command)
      (should (string= "abcd " (buffer-string))))))

(ert-deftest company-no-auto-complete-when-idle ()
  (with-temp-buffer
    (insert "ab")
    (company-mode)
    (let (company-frontends
          (company-auto-complete 'company-explicit-action-p)
          (company-auto-complete-chars '(? ))
          (company-backends
           (list (lambda (command &optional arg)
                   (case command
                     (prefix (buffer-substring (point-min) (point)))
                     (candidates '("abcd" "abef")))))))
      (company-idle-begin (current-buffer) (selected-window)
                          (buffer-chars-modified-tick) (point))
      (let ((last-command-event ? ))
        (self-insert-command 1))
      (company-post-command)
      (should (string= "ab " (buffer-string))))))
