;;; company-tests.el --- company-mode tests  -*- lexical-binding: t -*-

;; Copyright (C) 2011, 2013-2014  Free Software Foundation, Inc.

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
(require 'company-clang)

;;; Core

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
           (list (lambda (command &optional arg)
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

(ert-deftest company-multi-backend-with-lambdas ()
  (let ((company-backend
         (list (lambda (command &optional arg &rest ignore)
                 (cl-case command
                   (prefix "z")
                   (candidates '("a" "b"))))
               (lambda (command &optional arg &rest ignore)
                 (cl-case command
                   (prefix "z")
                   (candidates '("c" "d")))))))
    (should (equal (company-call-backend 'candidates "z") '("a" "b" "c" "d")))))

(ert-deftest company-multi-backend-remembers-candidate-backend ()
  (let ((company-backend
         (list (lambda (command &optional arg)
                 (cl-case command
                   (ignore-case nil)
                   (annotation "1")
                   (candidates '("a" "c"))
                   (post-completion "13")))
               (lambda (command &optional arg)
                 (cl-case command
                   (ignore-case t)
                   (annotation "2")
                   (candidates '("b" "d"))
                   (post-completion "42")))
               (lambda (command &optional arg)
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
  (let ((primo (lambda (command &optional arg)
                 (cl-case command
                   (prefix "a")
                   (candidates '("abb" "abc" "abd")))))
        (secundo (lambda (command &optional arg)
                   (cl-case command
                     (prefix "a")
                     (candidates '("acc" "acd"))))))
    (let ((company-backend (list 'ignore 'ignore :with secundo)))
      (should (null (company-call-backend 'prefix))))
    (let ((company-backend (list 'ignore primo :with secundo)))
      (should (equal "a" (company-call-backend 'prefix)))
      (should (equal '("abb" "abc" "abd" "acc" "acd")
                     (company-call-backend 'candidates "a"))))))

(ert-deftest company-begin-backend-failure-doesnt-break-company-backends ()
  (with-temp-buffer
    (insert "a")
    (company-mode)
    (should-error
     (company-begin-backend (lambda (command &rest ignore))))
    (let (company-frontends
          (company-backends
           (list (lambda (command &optional arg)
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
           (list (lambda (command &optional arg)
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
          (company-require-match 'company-explicit-action-p)
          (company-backends
           (list (lambda (command &optional arg)
                   (cl-case command
                     (prefix (buffer-substring (point-min) (point)))
                     (candidates '("abc" "abd")))))))
      (company-idle-begin (current-buffer) (selected-window)
                          (buffer-chars-modified-tick) (point))
      (let ((last-command-event ?e))
        (company-call 'self-insert-command 1))
      (should (eq nil company-candidates-length))
      (should (eq 4 (point))))))

(ert-deftest company-should-complete-whitelist ()
  (with-temp-buffer
    (insert "ab")
    (company-mode)
    (let (company-frontends
          company-begin-commands
          (company-backends
           (list (lambda (command &optional arg)
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
           (list (lambda (command &optional arg)
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

(ert-deftest company-auto-complete-explicit ()
  (with-temp-buffer
    (insert "ab")
    (company-mode)
    (let (company-frontends
          (company-auto-complete 'company-explicit-action-p)
          (company-auto-complete-chars '(? ))
          (company-backends
           (list (lambda (command &optional arg)
                   (cl-case command
                     (prefix (buffer-substring (point-min) (point)))
                     (candidates '("abcd" "abef")))))))
      (let (this-command)
        (company-complete))
      (let ((last-command-event ? ))
        (company-call 'self-insert-command 1))
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
          company-end-of-buffer-workaround
          (company-backends
           (list (lambda (command &optional arg)
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
           (list (lambda (command &optional arg)
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
          company-end-of-buffer-workaround
          (company-backends
           (list (lambda (command &optional arg)
                   (cl-case command
                     (prefix (buffer-substring (point-min) (point)))
                     (candidates '("tea-cup" "teal-color")))))))
      (let (this-command)
        (company-complete))
      (should (string= "tc" (buffer-string)))
      (company-complete-selection)
      (should (string= "tea-cup" (buffer-string))))))

(ert-deftest company-pseudo-tooltip-does-not-get-displaced ()
  :tags '(interactive)
  (with-temp-buffer
    (save-window-excursion
      (set-window-buffer nil (current-buffer))
      (save-excursion (insert " ff"))
      (company-mode)
      (let ((company-frontends '(company-pseudo-tooltip-frontend))
            (company-begin-commands '(self-insert-command))
            (company-backends
             (list (lambda (c &optional arg)
                     (cl-case c (prefix "") (candidates '("a" "b" "c")))))))
        (let (this-command)
          (company-call 'complete))
        (company-call 'open-line 1)
        (should (eq 2 (overlay-start company-pseudo-tooltip-overlay)))))))

(ert-deftest company-pseudo-tooltip-show ()
  :tags '(interactive)
  (with-temp-buffer
    (save-window-excursion
    (set-window-buffer nil (current-buffer))
    (insert "aaaa\n  bb\nccccccc\nddd")
    (search-backward "bb")
    (let ((col (company--column))
          (company-candidates-length 2)
          (company-candidates '("123" "45"))
          (company-backend 'ignore))
      (company-pseudo-tooltip-show (company--row) col 0)
      (let ((ov company-pseudo-tooltip-overlay))
        ;; With margins.
        (should (eq (overlay-get ov 'company-width) 5))
        ;; FIXME: Make it 2?
        (should (eq (overlay-get ov 'company-height) company-tooltip-limit))
        (should (eq (overlay-get ov 'company-column) col))
        (should (string= (overlay-get ov 'company-after)
                         "  123 \nc 45  c\nddd\n")))))))

(ert-deftest company-preview-show-with-annotations ()
  :tags '(interactive)
  (with-temp-buffer
    (save-window-excursion
      (set-window-buffer nil (current-buffer))
      (save-excursion (insert "\n"))
      (let ((company-candidates-length 1)
            (company-candidates '("123")))
        (company-preview-show-at-point (point))
        (let ((ov company-preview-overlay))
          (should (string= (overlay-get ov 'display) "123\n")))))))

(ert-deftest company-pseudo-tooltip-show-with-annotations ()
  :tags '(interactive)
  (with-temp-buffer
    (save-window-excursion
      (set-window-buffer nil (current-buffer))
      (insert " ")
      (save-excursion (insert "\n"))
      (let ((company-candidates-length 2)
            (company-backend (lambda (action &optional arg &rest _ignore)
                               (when (eq action 'annotation)
                                 (cdr (assoc arg '(("123" . "(4)")))))))
            (company-candidates '("123" "45"))
            company-tooltip-align-annotations)
        (company-pseudo-tooltip-show-at-point (point))
        (let ((ov company-pseudo-tooltip-overlay))
          ;; With margins.
          (should (eq (overlay-get ov 'company-width) 8))
          (should (string= (overlay-get ov 'company-after)
                           " 123(4) \n 45     \n")))))))

(ert-deftest company-pseudo-tooltip-show-with-annotations-right-aligned ()
  :tags '(interactive)
  (with-temp-buffer
    (save-window-excursion
      (set-window-buffer nil (current-buffer))
      (insert " ")
      (save-excursion (insert "\n"))
      (let ((company-candidates-length 3)
            (company-backend (lambda (action &optional arg &rest _ignore)
                               (when (eq action 'annotation)
                                 (cdr (assoc arg '(("123" . "(4)")
                                                   ("67" . "(891011)")))))))
            (company-candidates '("123" "45" "67"))
            (company-tooltip-align-annotations t))
        (company-pseudo-tooltip-show-at-point (point))
        (let ((ov company-pseudo-tooltip-overlay))
          ;; With margins.
          (should (eq (overlay-get ov 'company-width) 13))
          (should (string= (overlay-get ov 'company-after)
                           " 123     (4) \n 45          \n 67 (891011) \n")))))))

(ert-deftest company-create-lines-shows-numbers ()
  (let ((company-show-numbers t)
        (company-candidates '("x" "y" "z"))
        (company-candidates-length 3)
        (company-backend 'ignore))
    (should (equal '(" x 1 " " y 2 " " z 3 ")
                   (company--create-lines 0 999)))))

(ert-deftest company-create-lines-truncates-annotations ()
  (let* ((ww (company--window-width))
         (data `(("1" . "(123)")
                 ("2" . nil)
                 ("3" . ,(concat "(" (make-string (- ww 2) ?4) ")"))
                 (,(make-string ww ?4) . "<4>")))
         (company-candidates (mapcar #'car data))
         (company-candidates-length 4)
         (company-tooltip-margin 1)
         (company-backend (lambda (cmd &optional arg)
                            (when (eq cmd 'annotation)
                              (cdr (assoc arg data)))))
         company-tooltip-align-annotations)
    (should (equal (list (format " 1(123)%s " (company-space-string (- ww 8)))
                         (format " 2%s " (company-space-string (- ww 3)))
                         (format " 3(444%s " (make-string (- ww 7) ?4))
                         (format " %s " (make-string (- ww 2) ?4)))
                   (company--create-lines 0 999)))
    (let ((company-tooltip-align-annotations t))
      (should (equal (list (format " 1%s(123) " (company-space-string (- ww 8)))
                           (format " 2%s " (company-space-string (- ww 3)))
                           (format " 3 (444%s " (make-string (- ww 8) ?4))
                           (format " %s " (make-string (- ww 2) ?4)))
                     (company--create-lines 0 999))))))

(ert-deftest company-column-with-composition ()
  (with-temp-buffer
    (insert "lambda ()")
    (compose-region 1 (1+ (length "lambda")) "\\")
    (should (= (company--column) 4))))

(ert-deftest company-column-with-line-prefix ()
  (with-temp-buffer
    (insert "foo")
    (put-text-property (point-min) (point) 'line-prefix "  ")
    (should (= (company--column) 5))))

(ert-deftest company-column-wth-line-prefix-on-empty-line ()
  (with-temp-buffer
    (insert "\n")
    (forward-char -1)
    (put-text-property (point-min) (point-max) 'line-prefix "  ")
    (should (= (company--column) 2))))

(ert-deftest company-plainify ()
  (let ((tab-width 8))
    (should (equal-including-properties
             (company-plainify "\tabc\td\t")
             (concat "        "
                     "abc     "
                     "d       "))))
  (should (equal-including-properties
           (company-plainify (propertize "foobar" 'line-prefix "-*-"))
           "-*-foobar")))

(ert-deftest company-modify-line ()
  (let ((str "-*-foobar"))
    (should (equal-including-properties
             (company-modify-line str "zz" 4)
             "-*-fzzbar"))
    (should (equal-including-properties
             (company-modify-line str "xx" 0)
             "xx-foobar"))
    (should (equal-including-properties
             (company-modify-line str "zz" 10)
             "-*-foobar zz"))))

(ert-deftest company-scrollbar-bounds ()
  (should (equal nil (company--scrollbar-bounds 0 3 3)))
  (should (equal nil (company--scrollbar-bounds 0 4 3)))
  (should (equal '(0 . 0) (company--scrollbar-bounds 0 1 2)))
  (should (equal '(1 . 1) (company--scrollbar-bounds 2 2 4)))
  (should (equal '(2 . 3) (company--scrollbar-bounds 7 4 12)))
  (should (equal '(1 . 2) (company--scrollbar-bounds 3 4 12)))
  (should (equal '(1 . 3) (company--scrollbar-bounds 4 5 11))))

;;; Async

(defun company-async-backend (command &optional arg)
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
          (company-backends (list 'company-async-backend)))
      (company-manual-begin)
      (should (equal "foo" company-prefix))
      (should (equal '("abc" "abd") company-candidates)))))

(ert-deftest company-idle-begin-allows-async-candidates ()
  (with-temp-buffer
    (company-mode)
    (let (company-frontends
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
      (should (null company-prefix))
      (should (null company-candidates)))))

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
    (let* ((immediate (lambda (command &optional arg)
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
                                  (lambda (command &optional arg)
                                    (pcase command
                                      (`prefix "foo")
                                      (`candidates '("c" "d" "e"))))
                                  immediate)))
      (should (equal :async (car (company-call-backend-raw 'candidates "foo"))))
      (should (equal '("a" "b" "c" "d" "e" "f")
                     (company-call-backend 'candidates "foo")))
      (let ((company-backend (list immediate)))
        (should (equal '("f") (company-call-backend 'candidates "foo")))))))

;;; Template

(ert-deftest company-template-removed-after-the-last-jump ()
  (with-temp-buffer
    (insert "{ }")
    (goto-char 2)
    (let ((tpl (company-template-declare-template (point) (1- (point-max)))))
      (save-excursion
        (dotimes (i 2)
          (insert " ")
          (company-template-add-field tpl (point) "foo")))
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
        (insert " ")
        (company-template-add-field tpl (point) "bar"))
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

(defun company-call (name &rest args)
  (let* ((maybe (intern (format "company-%s" name)))
         (command (if (fboundp maybe) maybe name)))
    (let ((this-command command))
      (run-hooks 'pre-command-hook))
    (apply command args)
    (let ((this-command command))
      (run-hooks 'post-command-hook))))

(ert-deftest company-template-c-like-templatify ()
  (with-temp-buffer
    (let ((text "foo(int a, short b)"))
      (insert text)
      (company-template-c-like-templatify text)
      (should (equal "foo(arg0, arg1)" (buffer-string)))
      (should (looking-at "arg0"))
      (should (equal "int a"
                     (overlay-get (company-template-field-at) 'display))))))

(ert-deftest company-template-c-like-templatify-trims-after-closing-paren ()
  (with-temp-buffer
    (let ((text "foo(int a, short b)!@ #1334 a"))
      (insert text)
      (company-template-c-like-templatify text)
      (should (equal "foo(arg0, arg1)" (buffer-string)))
      (should (looking-at "arg0")))))

;;; Clang

(ert-deftest company-clang-objc-templatify ()
  (with-temp-buffer
    (let ((text "createBookWithTitle:andAuthor:"))
      (insert text)
      (company-clang-objc-templatify text)
      (should (equal "createBookWithTitle:arg0 andAuthor:arg1" (buffer-string)))
      (should (looking-at "arg0"))
      (should (null (overlay-get (company-template-field-at) 'display))))))
