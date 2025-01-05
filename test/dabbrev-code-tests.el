;;; dabbrev-code-tests.el --- company-mode tests  -*- lexical-binding: t -*-

;; Copyright (C) 2024-2025  Free Software Foundation, Inc.

;; Author: Denis Zubarev

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
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

(require 'company-tests)

(ert-deftest company-dabbrev-code-with-flex-style-test ()
  (skip-unless (version<= "27.0" emacs-version))
  (with-temp-buffer
    (insert "scheduled_job
sa_enum
self

se")
    (company-mode)
    (let (company-frontends
          company-transformers
          (company-dabbrev-code-other-buffers nil)
          (company-dabbrev-code-modes t)
          (company-backends '(company-dabbrev-code))
          (company-dabbrev-code-completion-styles '(flex)))
      (ignore company-dabbrev-code-other-buffers
              company-dabbrev-code-modes
              company-dabbrev-code-completion-styles)

      (company-manual-begin)
      (should (equal '("self" "sa_enum" "scheduled_job") company-candidates)))))

(ert-deftest company-dabbrev-code-with-basic-style-test ()
  (with-temp-buffer
    (insert "scheduled_job
sa_enum
self

se")
    (company-mode)
    (let (company-frontends
          company-transformers
          (company-dabbrev-code-other-buffers nil)
          (company-dabbrev-code-modes t)
          (company-backends '(company-dabbrev-code))
          (company-dabbrev-code-completion-styles '(basic)))
      (ignore company-dabbrev-code-other-buffers
              company-dabbrev-code-modes
              company-dabbrev-code-completion-styles)
      (company-manual-begin)
      (should (equal '("self") company-candidates)))))

(ert-deftest company-dabbrev-code-with-substring-style ()
  (skip-unless (version<= "27.0" emacs-version))
  (with-temp-buffer
    (insert "scheduled_job
ssa_enum
_sa_e
self

sa")
    (company-mode)
    (let (company-frontends
          company-transformers
          (company-dabbrev-code-other-buffers nil)
          (company-dabbrev-code-modes t)
          (company-backends '(company-dabbrev-code))
          (company-dabbrev-code-completion-styles '(substring)))
      (ignore company-dabbrev-code-other-buffers
              company-dabbrev-code-modes
              company-dabbrev-code-completion-styles)

      (company-manual-begin)
      (should (equal '("_sa_e" "ssa_enum") company-candidates)))))
