;;; cmake-tests.el --- company-mode tests  -*- lexical-binding: t -*-

;; Copyright (C) 2016  Free Software Foundation, Inc.

;; Author: Zuogong Yue

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
(require 'company-cmake)

(require 'cmake-mode)


(ert-deftest company-cmake-complete-in-string-prefix-quotes ()
  (with-temp-buffer
    (insert "set(MyFlags \"${CMAKE_CXX_FLAGS_R")
    (setq-local major-mode 'cmake-mode)
    (should (equal (company-cmake 'prefix)
                   "CMAKE_CXX_FLAGS_R"))
    (should (equal (company-cmake 'candidates "CMAKE_CXX_FLAGS_R")
                   '("CMAKE_CXX_FLAGS_RELWITHDEBINFO" "CMAKE_CXX_FLAGS_RELEASE")))))


(ert-deftest company-cmake-complete-in-string-between-quotes ()
  (with-temp-buffer
    (insert "set(MyFlags \"${CMAKE_CXX_FLAGS_R}\"")
    (backward-char 2)
    (setq-local major-mode 'cmake-mode)
    (should (equal (company-cmake 'prefix)
                   "CMAKE_CXX_FLAGS_R"))
    (should (equal (company-cmake 'candidates "CMAKE_CXX_FLAGS_R")
                   '("CMAKE_CXX_FLAGS_RELWITHDEBINFO" "CMAKE_CXX_FLAGS_RELEASE")))))


(ert-deftest company-cmake-complete-in-string-more-prefix ()
  (with-temp-buffer
    (insert "set(MyFlags \"${CMAKE_CXX_FLAGS} ${CMAKE_CXX_FLAGS_R")
    (setq-local major-mode 'cmake-mode)
    (should (equal (company-cmake 'prefix)
                   "CMAKE_CXX_FLAGS_R"))
    (should (equal (company-cmake 'candidates "CMAKE_CXX_FLAGS_R")
                   '("CMAKE_CXX_FLAGS_RELWITHDEBINFO" "CMAKE_CXX_FLAGS_RELEASE")))))

(ert-deftest company-cmake-complete-in-string-more-prefix-2 ()
  (with-temp-buffer
    (insert "set(MyFlags \"${CMAKE_CXX_FLAGS} CMAKE_CXX_FLAGS_R")
    (setq-local major-mode 'cmake-mode)
    (should (equal (company-cmake 'prefix)
                   nil))))

;; (ert t)