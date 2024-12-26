;;; dabbrev-code-tests.el --- company-mode tests  -*- lexical-binding: t -*-

;; Copyright (C) 2024  Free Software Foundation, Inc.

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

(defmacro company-dabbrev-code-with-buffer (contents &rest body)
  (declare (indent 0) (debug (sexp &rest form)))
  `(with-temp-buffer
     (insert ,contents)
     (re-search-backward "|")
     (replace-match "")
     (company-mode)
     (let (company-frontends
           company-transformers
           (company-dabbrev-code-other-buffers nil)
           (company-dabbrev-code-modes t)
           (company-backends '(company-dabbrev-code)))
       (ignore company-dabbrev-code-other-buffers
               company-dabbrev-code-modes)
       ,@body)))

(ert-deftest company-dabbrev-code-with-flex-style-test ()
  (skip-unless (version<= "27.0" emacs-version))
  (company-dabbrev-code-with-buffer
    "scheduled_job
sa_enum
self

se|"

    (let ((company-dabbrev-code-completion-styles '(flex)))
      (ignore company-dabbrev-code-completion-styles)
      (company-manual-begin)
      (should (equal '("self" "sa_enum" "scheduled_job") company-candidates)))))

(ert-deftest company-dabbrev-code-with-basic-style-test ()
  (company-dabbrev-code-with-buffer
    "scheduled_job
sa_enum
self

se|"
    (let ((company-dabbrev-code-completion-styles '(basic)))
      (ignore company-dabbrev-code-completion-styles)
      (company-manual-begin)
      (should (equal '("self") company-candidates)))))


(ert-deftest company-dabbrev-code-ignore-cand-prefix-test-1 ()
  (skip-unless (version<= "27.0" emacs-version))
  (let ((company-dabbrev-code-ignore-cand-prefix "_")
        (company-dabbrev-code-completion-styles '(flex)))
    (ignore company-dabbrev-code-ignore-cand-prefix company-dabbrev-code-completion-styles)
    (company-dabbrev-code-with-buffer
      "set_metric
_set_metric
__set_metric

se|"
      (company-manual-begin)
      (should (eq (length company-candidates) 2))
      (should (equal '("set_metric" "_set_metric") company-candidates))
      (company-cancel)

      ;; prefix starts with ignore-cand-prefix
      (insert "\n_se")
      (company-manual-begin)
      (should (equal '("_set_metric" "__set_metric") company-candidates)))))

(ert-deftest company-dabbrev-code-ignore-cand-prefix-test-2 ()
  (skip-unless (version<= "27.0" emacs-version))
  (let ((company-dabbrev-code-ignore-cand-prefix "__?")
        (company-dabbrev-code-completion-styles '(flex)))
    (ignore company-dabbrev-code-ignore-cand-prefix company-dabbrev-code-completion-styles)
    (company-dabbrev-code-with-buffer
      "set_metric
_set_metric
__set_metric

se|"
      (company-manual-begin)
      (should (eq (length company-candidates) 3))
      (should (equal '("set_metric" "_set_metric" "__set_metric") company-candidates))
      (company-cancel)

      ;; prefix starts with ignore-cand-prefix
      (insert "\n__se")
      (company-manual-begin)
      (should (equal '("__set_metric") company-candidates)))))

(ert-deftest company-dabbrev-code-ignore-cand-prefix-test-3 ()
  (skip-unless (version<= "27.0" emacs-version))
  (let ((company-dabbrev-code-ignore-cand-prefix "__?\\|--?")
        (company-dabbrev-code-completion-styles '(flex)))
    (ignore company-dabbrev-code-ignore-cand-prefix company-dabbrev-code-completion-styles)
    (company-dabbrev-code-with-buffer
      "set_metric
_set_metric
--set_metric

se|"
      (company-manual-begin)
      (should (eq (length company-candidates) 3))
      (should (equal '("set_metric" "_set_metric" "--set_metric") company-candidates))
      (company-cancel)

      ;; prefix starts with ignore-cand-prefix
      (insert "\n-se")
      (company-manual-begin)
      (should (equal '("--set_metric") company-candidates)))))
