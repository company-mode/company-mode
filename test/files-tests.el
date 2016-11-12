;;; filtes-tests.el --- company-mode tests  -*- lexical-binding: t -*-

;; Copyright (C) 2016  Free Software Foundation, Inc.

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
(require 'company-files)

(ert-deftest company-files-candidates-normal ()
  (let (company-files--completion-cache)
    (should (member (expand-file-name "test/" company-dir)
                    (company-files 'candidates
                                   company-dir)))))

(ert-deftest company-files-candidates-normal-root ()
  (let (company-files--completion-cache)
    (should (member "/bin/"
                    (company-files 'candidates "/")))))

(ert-deftest company-files-candidates-excluding-dir ()
  (let ((company-files-exclusions '("test/"))
        company-files--completion-cache)
    (should-not (member (expand-file-name "test/" company-dir)
                        (company-files 'candidates
                                       company-dir)))))

(ert-deftest company-files-candidates-excluding-files ()
  (let ((company-files-exclusions '(".el"))
        company-files--completion-cache)
    (should-not (member (expand-file-name "company.el" company-dir)
                        (company-files 'candidates
                                       company-dir)))))
