;;; bbdb-tests.el --- company-mode tests  -*- lexical-binding: t -*-

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
(require 'company-bbdb)

(provide 'bbdb-com)

(ert-deftest company-bbdb-prefix-looks-in-header-value ()
  (with-temp-buffer
    (insert "To: J")
    (setq-local major-mode 'message-mode)
    (should (equal (company-bbdb 'prefix)
                   "J"))))

(ert-deftest company-bbdb-prefix-includes-space ()
  (with-temp-buffer
    (insert "To: John Sm")
    (setq-local major-mode 'message-mode)
    (should (equal (company-bbdb 'prefix)
                   "John Sm"))))

(ert-deftest company-bbdb-prefix-begins-after-comma-or-semi ()
  (with-temp-buffer
    (insert "To: John Smythe <jsm@y.the>, Jess C")
    (setq-local major-mode 'message-mode)
    (should (equal (company-bbdb 'prefix)
                   "Jess C"))))
