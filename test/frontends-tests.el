;;; frontends-tests.el --- company-mode tests  -*- lexical-binding: t -*-

;; Copyright (C) 2015, 2016  Free Software Foundation, Inc.

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
             (list (lambda (c &rest _)
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
        (should (string= (overlay-get ov 'company-display)
                         "  123 \nc 45  c\nddd\n")))))))

(ert-deftest company-pseudo-tooltip-edit-updates-width ()
  :tags '(interactive)
  (with-temp-buffer
    (set-window-buffer nil (current-buffer))
    (let ((company-candidates-length 5)
          (company-candidates '("123" "45" "67" "89" "1011"))
          (company-backend 'ignore)
          (company-tooltip-limit 4)
          (company-tooltip-offset-display 'scrollbar))
      (company-pseudo-tooltip-show (company--row)
                                   (company--column)
                                   0)
      (should (eq (overlay-get company-pseudo-tooltip-overlay 'company-width)
                  6))
      (company-pseudo-tooltip-edit 4)
      (should (eq (overlay-get company-pseudo-tooltip-overlay 'company-width)
                  7)))))

(ert-deftest company-preview-show-with-annotations ()
  :tags '(interactive)
  (with-temp-buffer
    (save-window-excursion
      (set-window-buffer nil (current-buffer))
      (save-excursion (insert "\n"))
      (let ((company-backend #'ignore))
        (company-preview-show-at-point (point) "123")
        (let* ((ov company-preview-overlay)
               (str (overlay-get ov 'after-string)))
          (should (string= str "123"))
          (should (eq (get-text-property 0 'cursor str) 1)))))))

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
        (company-pseudo-tooltip-show-at-point (point) 0)
        (let ((ov company-pseudo-tooltip-overlay))
          ;; With margins.
          (should (eq (overlay-get ov 'company-width) 8))
          (should (string= (overlay-get ov 'company-display)
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
        (company-pseudo-tooltip-show-at-point (point) 0)
        (let ((ov company-pseudo-tooltip-overlay))
          ;; With margins.
          (should (eq (overlay-get ov 'company-width) 13))
          (should (string= (overlay-get ov 'company-display)
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
         (company-backend (lambda (cmd &optional arg &rest _)
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

(ert-deftest company-create-lines-truncates-common-part ()
  (let* ((ww (company--window-width))
         (company-candidates-length 2)
         (company-tooltip-margin 1)
         (company-backend #'ignore))
    (let* ((company-common (make-string (- ww 3) ?1))
           (company-candidates `(,(concat company-common "2")
                                 ,(concat company-common "3"))))
      (should (equal (list (format " %s2 " (make-string (- ww 3) ?1))
                           (format " %s3 " (make-string (- ww 3) ?1)))
                     (company--create-lines 0 999))))
    (let* ((company-common (make-string (- ww 2) ?1))
           (company-candidates `(,(concat company-common "2")
                                 ,(concat company-common "3"))))
      (should (equal (list (format " %s " company-common)
                           (format " %s " company-common))
                     (company--create-lines 0 999))))
    (let* ((company-common (make-string ww ?1))
           (company-candidates `(,(concat company-common "2")
                                 ,(concat company-common "3")))
           (res (company--create-lines 0 999)))
      (should (equal (list (format " %s " (make-string (- ww 2) ?1))
                           (format " %s " (make-string (- ww 2) ?1)))
                     res))
      (should (equal '(company-tooltip-common-selection
                       company-tooltip-selection
                       company-tooltip)
                     (get-text-property (- ww 2) 'face
                                        (car res))))
      (should (equal '(company-tooltip-selection
                       company-tooltip)
                     (get-text-property (1- ww) 'face
                                        (car res))))
      )))

(ert-deftest company-create-lines-clears-out-non-printables ()
  :tags '(interactive)
  (let (company-show-numbers
        (company-candidates (list
                             (decode-coding-string "avalis\351e" 'utf-8)
                             "avatar"))
        (company-candidates-length 2)
        (company-backend 'ignore))
    (should (equal '(" avalis‗e "
                     " avatar   ")
                   (company--create-lines 0 999)))))

(ert-deftest company-create-lines-handles-multiple-width ()
  :tags '(interactive)
  (let (company-show-numbers
        (company-candidates '("蛙蛙蛙蛙" "蛙abc"))
        (company-candidates-length 2)
        (company-backend 'ignore))
    (should (equal '(" ﻿蛙﻿蛙﻿蛙﻿蛙 "
                     " ﻿蛙abc    ")
                   (company--create-lines 0 999)))))

(ert-deftest company-create-lines-handles-multiple-width-in-annotation ()
  (let* (company-show-numbers
         (alist '(("a" . " ︸") ("b" . " ︸︸")))
         (company-candidates (mapcar #'car alist))
         (company-candidates-length 2)
         (company-backend (lambda (c &optional a &rest _)
                            (when (eq c 'annotation)
                              (assoc-default a alist)))))
    (should (equal '(" a ﻿︸   "
                     " b ﻿︸﻿︸ ")
                   (company--create-lines 0 999)))))

(ert-deftest company-create-lines-with-multiple-width-and-keep-prefix ()
  :tags '(interactive)
  (let* (company-show-numbers
         (company-candidates '("MIRAI発売1カ月"
                               "MIRAI発売2カ月"))
         (company-candidates-length 2)
         (company-prefix "MIRAI発")
         (company-backend (lambda (c &rest _)
                            (pcase c
                              (`ignore-case 'keep-prefix)))))
    (should (equal '(" MIRAI﻿発﻿売1﻿カ﻿月 "
                     " MIRAI﻿発﻿売2﻿カ﻿月 ")
                   (company--create-lines 0 999)))))

(ert-deftest company-fill-propertize-truncates-search-highlight ()
  (let ((company-search-string "foo")
        (company-backend #'ignore)
        (company-prefix ""))
    (should (ert-equal-including-properties
             (company-fill-propertize "barfoo" nil 6 t nil nil)
             #("barfoo"
               0 3 (face (company-tooltip-selection company-tooltip) mouse-face (company-tooltip-mouse))
               3 6 (face (company-tooltip-search-selection company-tooltip-selection company-tooltip) mouse-face (company-tooltip-mouse)))))
    (should (ert-equal-including-properties
             (company-fill-propertize "barfoo" nil 5 t "" " ")
             #("barfo "
               0 3 (face (company-tooltip-selection company-tooltip) mouse-face (company-tooltip-mouse))
               3 5 (face (company-tooltip-search-selection company-tooltip-selection company-tooltip) mouse-face (company-tooltip-mouse))
               5 6 (face (company-tooltip-selection company-tooltip) mouse-face (company-tooltip-mouse)))))
    (should (ert-equal-including-properties
             (company-fill-propertize "barfoo" nil 3 t " " " ")
             #(" bar "
               0 5 (face (company-tooltip-selection company-tooltip) mouse-face (company-tooltip-mouse)))))))

(ert-deftest company-fill-propertize-overrides-face-property ()
  (let ((company-backend #'ignore)
        (company-prefix "")
        (str1 (propertize "str1" 'face 'foo))
        (str2 (propertize "str2" 'face 'foo)))
    (should (ert-equal-including-properties
             (company-fill-propertize str1 str2 8 nil nil nil)
             #("str1str2"
               0 4 (face (company-tooltip) mouse-face (company-tooltip-mouse))
               4 8 (face (company-tooltip-annotation company-tooltip)
                         mouse-face (company-tooltip-mouse)))))))

(ert-deftest company-fill-propertize-delegates-to-pre-render ()
  (let ((company-backend
         (lambda (command &rest args)
           (pcase command
             (`pre-render
              (propertize (car args)
                          'face (if (cadr args)
                                    'annotation
                                  'value))))))
        (company-prefix "")
        (str1 (propertize "str1" 'foo 'bar))
        (str2 (propertize "str2" 'foo 'bar)))
    (let ((res (company-fill-propertize str1 str2 8 nil nil nil)))
      ;; Could use `ert-equal-including-properties' as well.
      (should (eq (get-text-property 0 'foo res) 'bar))
      (should (eq (get-text-property 4 'foo res) 'bar))
      (should (equal (get-text-property 0 'face res)
                     '(value company-tooltip)))
      (should (equal (get-text-property 4 'face res)
                     '(annotation company-tooltip-annotation company-tooltip))))))

(ert-deftest company-column-with-composition ()
  :tags '(interactive)
  (with-temp-buffer
    (save-window-excursion
      (set-window-buffer nil (current-buffer))
      (insert "lambda ()")
      (compose-region 1 (1+ (length "lambda")) "\\")
      (should (= (company--column) 4)))))

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

(ert-deftest company-buffer-lines-with-lines-folded ()
  :tags '(interactive)
  (with-temp-buffer
    (insert (propertize "aaa\nbbb\nccc\nddd\n" 'display "aaa+\n"))
    (insert "eee\nfff\nggg")
    (should (equal (company-buffer-lines (point-min) (point-max))
                   '("aaa" "eee" "fff" "ggg")))))

(ert-deftest company-buffer-lines-with-multiline-display ()
  :tags '(interactive)
  (with-temp-buffer
    (insert (propertize "a" 'display "bbb\nccc\ndddd\n"))
    (insert "eee\nfff\nggg")
    (should (equal (company-buffer-lines (point-min) (point-max))
                   '("a" "" "" "eee" "fff" "ggg")))))

(ert-deftest company-buffer-lines-with-multiline-after-string-at-eob ()
  :tags '(interactive)
  (with-temp-buffer
    (insert "a\nb\nc\n")
    (let ((ov (make-overlay (point-max) (point-max) nil t t)))
      (overlay-put ov 'after-string "~\n~\n~"))
    (should (equal (company-buffer-lines (point-min) (point-max))
                   '("a" "b" "c")))))

(ert-deftest company-buffer-lines-with-line-wrapping ()
  :tags '(interactive)
  (with-temp-buffer
    (let ((ww (company--window-width)))
      (insert (make-string (* 3 ww) ?a))
      (should (equal (company-buffer-lines (point-min) (point-max))
                     (list (make-string ww ?a)
                           (make-string ww ?a)
                           (make-string ww ?a)))))))

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

(ert-deftest company-modify-line-with-invisible-prop ()
  (let ((str "-*-foobar")
        (buffer-invisibility-spec '((outline . t) t)))
    (put-text-property 1 2 'invisible 'foo str)
    (should (equal
             (company-modify-line str "zz" 4)
             "-*-fzzbar"))))

(ert-deftest company-scrollbar-bounds ()
  (should (equal nil (company--scrollbar-bounds 0 3 3)))
  (should (equal nil (company--scrollbar-bounds 0 4 3)))
  (should (equal '(0 . 0) (company--scrollbar-bounds 0 1 2)))
  (should (equal '(1 . 1) (company--scrollbar-bounds 2 2 4)))
  (should (equal '(2 . 3) (company--scrollbar-bounds 7 4 12)))
  (should (equal '(1 . 2) (company--scrollbar-bounds 3 4 12)))
  (should (equal '(1 . 3) (company--scrollbar-bounds 4 5 11))))
