;;; capf-tests.el --- company tests for the company-capf backend  -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Free Software Foundation, Inc.

;; Author: João Távora <joaotavora@gmail.com>
;; Keywords: 

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

(require 'company-tests)
(require 'company-capf)
(require 'cl-lib)

(defmacro company-capf-with-buffer (contents &rest body)
  (declare (indent 0) (debug (sexp &rest form)))
  `(with-temp-buffer
     (insert ,contents)
     (emacs-lisp-mode)
     (re-search-backward "|")
     (replace-match "")
     (let ((completion-at-point-functions '(elisp-completion-at-point))
           (company-backends '(company-capf)))
       ,@body)))

(ert-deftest company-basic-capf ()
  "Test basic `company-capf' support."
  (company-capf-with-buffer
    "(with-current-buffer|)"
    (company-mode)
    (company-complete)
    (should company-candidates)))

(ert-deftest company-non-prefix-capf ()
  "Test non-prefix `company-capf' in elisp"
  (company-capf-with-buffer
    "(w-c-b|)"
    (company-mode)
    (company-complete)
    (should company-candidates)
    (should (member "with-current-buffer" company-candidates))))

(defun company--remove-but-these-properties (string keep)
  "Remove from STRING all text properties but the ones in KEEP."
  (remove-list-of-text-properties
   0 (length string)
   (cl-set-difference
    (cl-loop for start = 0 then (next-property-change start string)
             while start
             append (cl-loop for (k _v) on (text-properties-at start string)
                             by #'cddr collect k))
    keep)
   string)
  string)

(ert-deftest company-basic-capf-highlighting ()
  "Test basic `company-capf' support, with basic prefix completion."
  (company-capf-with-buffer
    "(with|)"
    (company-mode)
    (company-complete)
    (should company-candidates)
    (let* ((cand (car (member "with-current-buffer" company-candidates)))
           (render
            (and cand
                 (company-fill-propertize cand nil (length cand) nil nil nil))))
      ;; remove text properties that aren't relevant to our test
      (company--remove-but-these-properties render '(face))
      (should
       (ert-equal-including-properties
        render
        #("with-current-buffer"
          0 4 (face (company-tooltip-common company-tooltip))   ; "with"
          4 19 (face (company-tooltip))))))))



;; Re. "perfect" highlighting of the non-prefix in company-capf matches, it is
;; only working-out-of-the box (i.e. without the `:company-match' meta) in
;; recent Emacsen containing the following commit.  The two tests that follow
;; reflect that.
;;
;; commit 325ef57b0e3977f9509f1049c826999e8b7c226d
;; Author: Stefan Monnier <monnier@iro.umontreal.ca>
;; Date:   Tue Nov 7 12:17:34 2017 -0500

(ert-deftest company-non-prefix-fancy-capf-highlighting ()
  "Test highlighting for non-prefix `company-capf' in elisp"
  (skip-unless (version<= "27.0" emacs-version))
  (company-capf-with-buffer
    "(w-c-b|)"
    (company-mode)
    (company-complete)
    (let* ((cand (car (member "with-current-buffer" company-candidates)))
           (render
            (and cand
                 (company-fill-propertize cand nil (length cand) nil nil nil))))
      ;; remove text properties that aren't relevant to our test
      (company--remove-but-these-properties render '(face))
      (should
       (ert-equal-including-properties
        render
        #("with-current-buffer"
          0 1 (face (company-tooltip-common company-tooltip))   ; "w"
          1 4 (face (company-tooltip))                          ; "ith"
          4 6 (face (company-tooltip-common company-tooltip))   ; "-c"
          6 12 (face (company-tooltip))                         ; "urrent"
          12 14 (face (company-tooltip-common company-tooltip)) ; "-b"
          14 19 (face (company-tooltip))))))))                  ; "uffer"

(ert-deftest company-non-prefix-modest-capf-highlighting ()
  "Test highlighting for non-prefix `company-capf' in elisp"
  (skip-unless (version< emacs-version "27.0"))
  (company-capf-with-buffer
    "(w-c-b|)"
    (company-mode)
    (company-complete)
    (let* ((cand (car (member "with-current-buffer" company-candidates)))
           (render
            (and cand
                 (company-fill-propertize cand nil (length cand) nil nil nil))))
      ;; remove text properties that aren't relevant to our test
      (company--remove-but-these-properties render '(face))
      (should
       (ert-equal-including-properties
        render
        #("with-current-buffer"
          0 14 (face (company-tooltip-common company-tooltip)); "with-current-b"
          14 19 (face (company-tooltip))))))))                ; "uffer"

(provide 'capf-tests)
;;; capf-tests.el ends here
