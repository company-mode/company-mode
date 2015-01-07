;;; transformers-tests.el --- company-mode tests  -*- lexical-binding: t -*-

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

(ert-deftest company-occurrence-prefer-closest-above ()
  (with-temp-buffer
    (save-window-excursion
      (set-window-buffer nil (current-buffer))
      (insert "foo0
foo1
")
      (save-excursion
        (insert "
foo3
foo2"))
      (let ((company-backend 'company-dabbrev)
            (company-occurrence-weight-function
             'company-occurrence-prefer-closest-above))
        (should (equal '("foo1" "foo0" "foo3" "foo2" "foo4")
                       (company-sort-by-occurrence
                        '("foo0" "foo1" "foo2" "foo3" "foo4"))))))))

(ert-deftest company-occurrence-prefer-any-closest ()
  (with-temp-buffer
    (save-window-excursion
      (set-window-buffer nil (current-buffer))
      (insert "foo0
foo1
")
      (save-excursion
        (insert "
foo3
foo2"))
      (let ((company-backend 'company-dabbrev)
            (company-occurrence-weight-function
             'company-occurrence-prefer-any-closest))
        (should (equal '("foo1" "foo3" "foo0" "foo2" "foo4")
                       (company-sort-by-occurrence
                        '("foo0" "foo1" "foo2" "foo3" "foo4"))))))))
