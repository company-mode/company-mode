;;; core-tests.el --- company-mode tests  -*- lexical-binding: t -*-

;; Copyright (C) 2015, 2016, 2017  Free Software Foundation, Inc.

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

(ert-deftest company-good-prefix ()
  (let ((company-minimum-prefix-length 5)
        company-abort-manual-when-too-short
        company--manual-action            ;idle begin
        (company-selection-changed t))    ;has no effect
    (should (eq t (company--good-prefix-p "!@#$%")))
    (should (eq nil (company--good-prefix-p "abcd")))
    (should (eq nil (company--good-prefix-p 'stop)))
    (should (eq t (company--good-prefix-p '("foo" . 5))))
    (should (eq nil (company--good-prefix-p '("foo" . 4))))
    (should (eq t (company--good-prefix-p '("foo" . t))))))

(ert-deftest company--manual-prefix-set-and-unset ()
  (with-temp-buffer
    (insert "ab")
    (company-mode)
    (let (company-frontends
          (company-backends
           (list (lambda (command &optional _)
                   (cl-case command
                     (prefix (buffer-substring (point-min) (point)))
                     (candidates '("abc" "abd")))))))
      (company-manual-begin)
      (should (equal "ab" company--manual-prefix))
      (company-abort)
      (should (null company--manual-prefix)))))

(ert-deftest company-abort-manual-when-too-short ()
  (let ((company-minimum-prefix-length 5)
        (company-abort-manual-when-too-short t)
        (company-selection-changed t))    ;has not effect
    (let ((company--manual-action nil))   ;idle begin
      (should (eq t (company--good-prefix-p "!@#$%")))
      (should (eq t (company--good-prefix-p '("foo" . 5))))
      (should (eq t (company--good-prefix-p '("foo" . t)))))
    (let ((company--manual-action t)
          (company--manual-prefix "abc")) ;manual begin from this prefix
      (should (eq t (company--good-prefix-p "!@#$")))
      (should (eq nil (company--good-prefix-p "ab")))
      (should (eq nil (company--good-prefix-p 'stop)))
      (should (eq t (company--good-prefix-p '("foo" . 4))))
      (should (eq t (company--good-prefix-p "abcd")))
      (should (eq t (company--good-prefix-p "abc")))
      (should (eq t (company--good-prefix-p '("bar" . t)))))))

(ert-deftest company-common-with-non-prefix-completion ()
  (let ((company-backend #'ignore)
        (company-prefix "abc")
        company-candidates
        company-candidates-length
        company-candidates-cache
        company-common)
    (company-update-candidates '("abc" "def-abc"))
    (should (null company-common))
    (company-update-candidates '("abc" "abe-c"))
    (should (null company-common))
    (company-update-candidates '("abcd" "abcde" "abcdf"))
    (should (equal "abcd" company-common))))

(ert-deftest company-multi-backend-with-lambdas ()
  (let ((company-backend
         (list (lambda (command &optional _ &rest _r)
                 (cl-case command
                   (prefix "z")
                   (candidates '("a" "b"))))
               (lambda (command &optional _ &rest _r)
                 (cl-case command
                   (prefix "z")
                   (candidates '("c" "d")))))))
    (should (equal (company-call-backend 'candidates "z") '("a" "b" "c" "d")))))

(ert-deftest company-multi-backend-filters-backends-by-prefix ()
  (let ((company-backend
         (list (lambda (command &optional _ &rest _r)
                 (cl-case command
                   (prefix (cons "z" t))
                   (candidates '("a" "b"))))
               (lambda (command &optional _ &rest _r)
                 (cl-case command
                   (prefix "t")
                   (candidates '("c" "d"))))
               (lambda (command &optional _ &rest _r)
                 (cl-case command
                   (prefix "z")
                   (candidates '("e" "f")))))))
    (should (equal (company-call-backend 'candidates "z") '("a" "b" "e" "f")))))

(ert-deftest company-multi-backend-remembers-candidate-backend ()
  (let ((company-backend
         (list (lambda (command &optional _)
                 (cl-case command
                   (ignore-case nil)
                   (annotation "1")
                   (candidates '("a" "c"))
                   (post-completion "13")))
               (lambda (command &optional _)
                 (cl-case command
                   (ignore-case t)
                   (annotation "2")
                   (candidates '("b" "d"))
                   (post-completion "42")))
               (lambda (command &optional _)
                 (cl-case command
                   (annotation "3")
                   (candidates '("e"))
                   (post-completion "74"))))))
    (let ((candidates (company-calculate-candidates nil)))
      (should (equal candidates '("a" "b" "c" "d" "e")))
      (should (equal t (company-call-backend 'ignore-case)))
      (should (equal "1" (company-call-backend 'annotation (nth 0 candidates))))
      (should (equal "2" (company-call-backend 'annotation (nth 1 candidates))))
      (should (equal "13" (company-call-backend 'post-completion (nth 2 candidates))))
      (should (equal "42" (company-call-backend 'post-completion (nth 3 candidates))))
      (should (equal "3" (company-call-backend 'annotation (nth 4 candidates))))
      (should (equal "74" (company-call-backend 'post-completion (nth 4 candidates)))))))

(ert-deftest company-multi-backend-handles-keyword-with ()
  (let ((primo (lambda (command &optional _)
                 (cl-case command
                   (prefix "a")
                   (candidates '("abb" "abc" "abd")))))
        (secundo (lambda (command &optional _)
                   (cl-case command
                     (prefix "a")
                     (candidates '("acc" "acd"))))))
    (let ((company-backend (list 'ignore 'ignore :with secundo)))
      (should (null (company-call-backend 'prefix))))
    (let ((company-backend (list 'ignore primo :with secundo)))
      (should (equal "a" (company-call-backend 'prefix)))
      (should (equal '("abb" "abc" "abd" "acc" "acd")
                     (company-call-backend 'candidates "a"))))))

(ert-deftest company-multi-backend-handles-keyword-separate ()
  (let ((one (lambda (command &optional _)
               (cl-case command
                 (prefix "a")
                 (candidates (list "aa" "ca" "ba")))))
        (two (lambda (command &optional _)
               (cl-case command
                 (prefix "a")
                 (candidates (list "bb" "ab")))))
        (tri (lambda (command &optional _)
               (cl-case command
                 (prefix "a")
                 (sorted t)
                 (candidates (list "cc" "bc" "ac"))))))
    (let ((company-backend (list one two tri :separate)))
      (should (company-call-backend 'sorted))
      (should-not (company-call-backend 'duplicates))
      (should (equal '("aa" "ba" "ca" "ab" "bb" "cc" "bc" "ac")
                     (company-call-backend 'candidates "a"))))))

(ert-deftest company-begin-backend-failure-doesnt-break-company-backends ()
  (with-temp-buffer
    (insert "a")
    (company-mode)
    (should-error
     (company-begin-backend #'ignore))
    (let (company-frontends
          (company-backends
           (list (lambda (command &optional _)
                   (cl-case command
                     (prefix "a")
                     (candidates '("a" "ab" "ac")))))))
      (let (this-command)
        (company-call 'complete))
      (should (eq 3 company-candidates-length)))))

(ert-deftest company-require-match-explicit ()
  (with-temp-buffer
    (insert "ab")
    (company-mode)
    (let (company-frontends
          (company-require-match 'company-explicit-action-p)
          (company-backends
           (list (lambda (command &optional _)
                   (cl-case command
                     (prefix (buffer-substring (point-min) (point)))
                     (candidates '("abc" "abd")))))))
      (let (this-command)
        (company-complete))
      (let ((last-command-event ?e))
        (company-call 'self-insert-command 1))
      (should (eq 2 company-candidates-length))
      (should (eq 3 (point))))))

(ert-deftest company-dont-require-match-when-idle ()
  (with-temp-buffer
    (insert "ab")
    (company-mode)
    (let (company-frontends
          (company-minimum-prefix-length 2)
          (company-require-match 'company-explicit-action-p)
          (company-backends
           (list (lambda (command &optional _)
                   (cl-case command
                     (prefix (buffer-substring (point-min) (point)))
                     (candidates '("abc" "abd")))))))
      (company-idle-begin (current-buffer) (selected-window)
                          (buffer-chars-modified-tick) (point))
      (should (eq 2 company-candidates-length))
      (let ((last-command-event ?e))
        (company-call 'self-insert-command 1))
      (should (eq nil company-candidates-length))
      (should (eq 4 (point))))))

(ert-deftest company-dont-require-match-if-was-a-match-and-old-prefix-ended ()
  (with-temp-buffer
    (insert "ab")
    (company-mode)
    (let (company-frontends
          company-auto-complete
          (company-require-match t)
          (company-backends
           (list (lambda (command &optional _)
                   (cl-case command
                     (prefix (company-grab-word))
                     (candidates '("abc" "ab" "abd"))
                     (sorted t))))))
      (let (this-command)
        (company-complete))
      (let ((last-command-event ?e))
        (company-call 'self-insert-command 1))
      (should (eq 3 company-candidates-length))
      (should (eq 3 (point)))
      (let ((last-command-event ? ))
        (company-call 'self-insert-command 1))
      (should (null company-candidates-length))
      (should (eq 4 (point))))))

(ert-deftest company-dont-require-match-if-was-a-match-and-new-prefix-is-stop ()
  (with-temp-buffer
    (company-mode)
    (insert "c")
    (let (company-frontends
          (company-require-match t)
          (company-backends
           (list (lambda (command &optional _)
                   (cl-case command
                     (prefix (if (> (point) 2)
                                 'stop
                               (buffer-substring (point-min) (point))))
                     (candidates '("a" "b" "c")))))))
      (let (this-command)
        (company-complete))
      (should (eq 3 company-candidates-length))
      (let ((last-command-event ?e))
        (company-call 'self-insert-command 1))
      (should (not company-candidates)))))

(ert-deftest company-should-complete-whitelist ()
  (with-temp-buffer
    (insert "ab")
    (company-mode)
    (let (company-frontends
          company-begin-commands
          (company-backends
           (list (lambda (command &optional _)
                   (cl-case command
                     (prefix (buffer-substring (point-min) (point)))
                     (candidates '("abc" "abd")))))))
      (let ((company-continue-commands nil))
        (let (this-command)
          (company-complete))
        (company-call 'backward-delete-char 1)
        (should (null company-candidates-length)))
      (let ((company-continue-commands '(backward-delete-char)))
        (let (this-command)
          (company-complete))
        (company-call 'backward-delete-char 1)
        (should (eq 2 company-candidates-length))))))

(ert-deftest company-should-complete-blacklist ()
  (with-temp-buffer
    (insert "ab")
    (company-mode)
    (let (company-frontends
          company-begin-commands
          (company-backends
           (list (lambda (command &optional _)
                   (cl-case command
                     (prefix (buffer-substring (point-min) (point)))
                     (candidates '("abc" "abd")))))))
      (let ((company-continue-commands '(not backward-delete-char)))
        (let (this-command)
          (company-complete))
        (company-call 'backward-delete-char 1)
        (should (null company-candidates-length)))
      (let ((company-continue-commands '(not backward-delete-char-untabify)))
        (let (this-command)
          (company-complete))
        (company-call 'backward-delete-char 1)
        (should (eq 2 company-candidates-length))))))

(ert-deftest company-backspace-into-bad-prefix ()
  (with-temp-buffer
    (insert "ab")
    (company-mode)
    (let (company-frontends
          (company-minimum-prefix-length 2)
          (company-backends
           (list (lambda (command &optional _)
                   (cl-case command
                     (prefix (buffer-substring (point-min) (point)))
                     (candidates '("abcd" "abef")))))))
      (let ((company-idle-delay 'now))
        (company-auto-begin))
      (company-call 'backward-delete-char-untabify 1)
      (should (string= "a" (buffer-string)))
      (should (null company-candidates)))))

(ert-deftest company-auto-complete-explicit ()
  (with-temp-buffer
    (insert "ab")
    (company-mode)
    (let (company-frontends
          (company-auto-complete 'company-explicit-action-p)
          (company-auto-complete-chars '(? ))
          (company-backends
           (list (lambda (command &optional _)
                   (cl-case command
                     (prefix (buffer-substring (point-min) (point)))
                     (candidates '("abcd" "abef")))))))
      (let (this-command)
        (company-complete))
      (let ((last-command-event ? ))
        (company-call 'self-insert-command 1))
      (should (string= "abcd " (buffer-string))))))

(ert-deftest company-auto-complete-with-electric-pair ()
  (with-temp-buffer
    (insert "foo(ab)")
    (forward-char -1)
    (company-mode)
    (let (company-frontends
          (company-auto-complete t)
          (company-auto-complete-chars '(? ?\)))
          (company-backends
           (list (lambda (command &optional _)
                   (cl-case command
                     (prefix (buffer-substring 5 (point)))
                     (candidates '("abcd" "abef"))))))
          (electric-pair electric-pair-mode))
      (unwind-protect
          (progn
            (electric-pair-mode)
            (let (this-command)
              (company-complete))
            (let ((last-command-event ?\)))
              (company-call 'self-insert-command 1)))
        (unless electric-pair
          (electric-pair-mode -1)))
      (should (string= "foo(abcd)" (buffer-string))))))

(ert-deftest company-no-auto-complete-when-idle ()
  (with-temp-buffer
    (insert "ab")
    (company-mode)
    (let (company-frontends
          (company-auto-complete 'company-explicit-action-p)
          (company-auto-complete-chars '(? ))
          (company-minimum-prefix-length 2)
          (company-backends
           (list (lambda (command &optional _)
                   (cl-case command
                     (prefix (buffer-substring (point-min) (point)))
                     (candidates '("abcd" "abef")))))))
      (company-idle-begin (current-buffer) (selected-window)
                          (buffer-chars-modified-tick) (point))
      (let ((last-command-event ? ))
        (company-call 'self-insert-command 1))
      (should (string= "ab " (buffer-string))))))

(ert-deftest company-clears-explicit-action-when-no-matches ()
  (with-temp-buffer
    (company-mode)
    (let (company-frontends
          company-backends)
      (company-call 'manual-begin) ;; fails
      (should (null company-candidates))
      (should (null (company-explicit-action-p))))))

(ert-deftest company-ignore-case-replaces-prefix ()
  (with-temp-buffer
    (company-mode)
    (let (company-frontends
          (company-backends
           (list (lambda (command &optional _)
                   (cl-case command
                     (prefix (buffer-substring (point-min) (point)))
                     (candidates '("abcd" "abef"))
                     (ignore-case t))))))
      (insert "A")
      (let (this-command)
        (company-complete))
      (should (string= "ab" (buffer-string)))
      (delete-char -2)
      (insert "A") ; hack, to keep it in one test
      (company-complete-selection)
      (should (string= "abcd" (buffer-string))))))

(ert-deftest company-ignore-case-with-keep-prefix ()
  (with-temp-buffer
    (insert "AB")
    (company-mode)
    (let (company-frontends
          (company-backends
           (list (lambda (command &optional _)
                   (cl-case command
                     (prefix (buffer-substring (point-min) (point)))
                     (candidates '("abcd" "abef"))
                     (ignore-case 'keep-prefix))))))
      (let (this-command)
        (company-complete))
      (company-complete-selection)
      (should (string= "ABcd" (buffer-string))))))

(ert-deftest company-non-prefix-completion ()
  (with-temp-buffer
    (insert "tc")
    (company-mode)
    (let (company-frontends
          (company-backends
           (list (lambda (command &optional _)
                   (cl-case command
                     (prefix (buffer-substring (point-min) (point)))
                     (candidates '("tea-cup" "teal-color")))))))
      (let (this-command)
        (company-complete))
      (should (string= "tc" (buffer-string)))
      (company-complete-selection)
      (should (string= "tea-cup" (buffer-string))))))

(defvar ct-sorted nil)

(defun ct-equal-including-properties (list1 list2)
  (or (and (not list1) (not list2))
      (and (ert-equal-including-properties (car list1) (car list2))
           (ct-equal-including-properties (cdr list1) (cdr list2)))))

(ert-deftest company-strips-duplicates-returns-nil ()
  (should (null (company--preprocess-candidates nil))))

(ert-deftest company-strips-duplicates-within-groups ()
  (let* ((kvs '(("a" . "b")
                ("a" . nil)
                ("a" . "b")
                ("a" . "c")
                ("a" . "b")
                ("b" . "c")
                ("b" . nil)
                ("a" . "b")))
         (fn (lambda (kvs)
               (mapcar (lambda (kv) (propertize (car kv) 'ann (cdr kv)))
                       kvs)))
         (company-backend
          (lambda (command &optional arg)
            (pcase command
              (`prefix "")
              (`sorted ct-sorted)
              (`duplicates t)
              (`annotation (get-text-property 0 'ann arg)))))
         (reference '(("a" . "b")
                      ("a" . nil)
                      ("a" . "c")
                      ("b" . "c")
                      ("b" . nil)
                      ("a" . "b"))))
    (let ((ct-sorted t))
      (should (ct-equal-including-properties
               (company--preprocess-candidates (funcall fn kvs))
               (funcall fn reference))))
    (should (ct-equal-including-properties
             (company--preprocess-candidates (funcall fn kvs))
             (funcall fn (butlast reference))))))

;;; Row and column

(ert-deftest company-column-with-composition ()
  :tags '(interactive)
  (with-temp-buffer
    (save-window-excursion
      (set-window-buffer nil (current-buffer))
      (insert "lambda ()")
      (compose-region 1 (1+ (length "lambda")) "\\")
      (should (= (company--column) 4)))))

(ert-deftest company-column-with-line-prefix ()
  :tags '(interactive)
  (with-temp-buffer
    (save-window-excursion
      (set-window-buffer nil (current-buffer))
      (insert "foo")
      (put-text-property (point-min) (point) 'line-prefix "  ")
      (should (= (company--column) 5)))))

(ert-deftest company-column-with-line-prefix-on-empty-line ()
  :tags '(interactive)
  (with-temp-buffer
    (save-window-excursion
      (set-window-buffer nil (current-buffer))
      (insert "\n")
      (forward-char -1)
      (put-text-property (point-min) (point-max) 'line-prefix "  ")
      (should (= (company--column) 2)))))

(ert-deftest company-column-with-tabs ()
  :tags '(interactive)
  (with-temp-buffer
    (save-window-excursion
      (set-window-buffer nil (current-buffer))
      (insert "|\t|\t|\t(")
      (let ((tab-width 8))
        (should (= (company--column) 25))))))

(ert-deftest company-row-with-header-line-format ()
  :tags '(interactive)
  (with-temp-buffer
    (save-window-excursion
      (set-window-buffer nil (current-buffer))
      (should (= (company--row) 0))
      (setq header-line-format "aaaaaaa")
      (should (= (company--row) 0)))))
