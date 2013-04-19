;;; company-cmake.el --- company-mode completion back-end for cmake-mode

;; Copyright (C) 2013  Free Software Foundation, Inc.

;; Author: Chen Bin <chenbin DOT sh AT gmail>

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
;; company-cmake will auto-complete module names, variable names and commands of CMake
;;
;;; Change Log:
;;
;; 2013-04-20: first import

;;; Code:
(require 'company)

(defgroup company-cmake nil
  "Completion back-end for cmake-mode."
  :group 'company)

(defcustom company-cmake-executable
  (executable-find "cmake")
  "Location of cmake executable."
  :type 'file-attributes)

(defvar company-cmake-executable-arguments
  '("--help-command-list"
    "--help-module-list"
    "--help-variable-list"
    )
  "the arguments of cmake executable which decide the commands/variable/modules we need auto-complete")
(defvar company-cmake--completion-pattern
  "^\\(%s[a-zA-Z0-9_]*\\)$"
  "pattern to much candidates")

(defvar company-cmake-modes '(cmake-mode)
  "Major modes which cmake may complete.")

(defvar company-cmake--meta-cache nil)

(defun company-cmake--parse-output (prefix)
  "analyze the temp-buffer and output lines"
  (goto-char (point-min))
  (let ((pattern (format company-cmake--completion-pattern
                         (regexp-quote prefix)))
        (case-fold-search nil)
        lines match
        )
    (while (re-search-forward pattern nil t)
      (setq match (match-string-no-properties 1))
      (push match lines))
    lines))

(defun company-cmake--candidates (prefix)
  (with-temp-buffer
    (let ((res (apply 'call-process company-cmake-executable nil t nil company-cmake-executable-arguments)))
      (unless (eq 0 res)
        (messsage "cmake executable exited with error=%d" res))
      (company-cmake--parse-output prefix)))
  )

(defun company-cmake (command &optional arg &rest ignored)
  "`company-mode' completion back-end for CMake.
CMake is a cross-platform, open-source make system."
  (interactive (list 'interactive))
  (case command
    (interactive (company-begin-backend 'company-cmake))
    (init (when (memq major-mode company-cmake-modes)
            (unless company-cmake-executable
              (error "Company found no cmake executable"))
            ))
    (prefix (and (memq major-mode company-cmake-modes)
                 (not (company-in-string-or-comment))
                 (company-grab-symbol)))
    (candidates (company-cmake--candidates arg))
    (meta (format "This value is named %s" arg))
    )
  )

(provide 'company-cmake)
;;; company-cmake.el ends here