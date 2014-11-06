;;; company-cmake.el --- company-mode completion back-end for CMake

;; Copyright (C) 2013  Free Software Foundation, Inc.

;; Author: Chen Bin <chenbin DOT sh AT gmail>
;; Version: 0.2

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; company-cmake offers completions for module names, variable names and
;; commands used by CMake.  And their descriptions.

;;; Code:

(require 'company)
(require 'cl-lib)

(defgroup company-cmake nil
  "Completion back-end for CMake."
  :group 'company)

(defcustom company-cmake-executable
  (executable-find "cmake")
  "Location of cmake executable."
  :type 'file)

(defvar company-cmake-executable-arguments
  '("--help-command-list"
    "--help-module-list"
    "--help-variable-list")
  "The arguments we pass to cmake, separately.
They affect which types of symbols we get completion candidates for.")

(defvar company-cmake--completion-pattern
  "^\\(%s[a-zA-Z0-9_<>]%s\\)$"
  "Regexp to match the candidates.")

(defvar company-cmake-modes '(cmake-mode)
  "Major modes in which cmake may complete.")

(defvar company-cmake--candidates-cache nil
  "Cache for the raw candidates.")

(defvar company-cmake--meta-command-cache nil
  "Cache for command arguments to retrieve descriptions for the candidates.")

(defun company-cmake--replace-tags (rlt)
  (setq rlt (replace-regexp-in-string
             "\\(.*\\)<LANG>\\(.*\\)"
             (mapconcat 'identity '("\\1CXX\\2" "\\1C\\2" "\\1G77\\2") "\n")
             rlt))
  (setq rlt (replace-regexp-in-string
             "\\(.*\\)<CONFIG>\\(.*\\)"
             (mapconcat 'identity '("\\1DEBUG\\2" "\\1RELEASE\\2"
                                    "\\1RELWITHDEBINFO\\2" "\\1MINSIZEREL\\2")
                        "\n")
             rlt))
  rlt)

(defun company-cmake--fill-candidates-cache (arg)
  "Fill candidates cache if needed."
  (let (rlt)
    (unless company-cmake--candidates-cache
      (setq company-cmake--candidates-cache (make-hash-table :test 'equal)))

    ;; If hash is empty, fill it.
    (unless (gethash arg company-cmake--candidates-cache)
      (with-temp-buffer
        (let ((res 0))
          (setq res (call-process company-cmake-executable nil t nil arg))
          (unless (eq 0 res)
            (message "cmake executable exited with error=%d" res)))
        (setq rlt (buffer-string)))
      (setq rlt (company-cmake--replace-tags rlt))
      (puthash arg rlt company-cmake--candidates-cache))
    ))

(defun company-cmake--find-match (pattern line cmd)
  (let (match)
     ;; General Flags
     (if (string-match pattern line)
      (if (setq match (match-string 1 line))
        (puthash match cmd company-cmake--meta-command-cache)))
    match))

(defun company-cmake--parse (prefix content cmd)
  (let ((start 0)
        (pattern (format company-cmake--completion-pattern
                         (regexp-quote prefix)
                         (if (zerop (length prefix)) "+" "*")))
        (lines (split-string content "\n"))
        match
        rlt)
    (dolist (line lines)
      (if (setq match (company-cmake--find-match pattern line cmd))
          (push match rlt)))
    rlt))

(defun company-cmake--candidates (prefix)
  (let (results
        cmd-opts
        str)

    (unless company-cmake--meta-command-cache
      (setq company-cmake--meta-command-cache (make-hash-table :test 'equal)))

    (dolist (arg company-cmake-executable-arguments)
      (company-cmake--fill-candidates-cache arg)
      (setq cmd-opts (replace-regexp-in-string "-list$" "" arg) )

      (setq str (gethash arg company-cmake--candidates-cache))
      (when str
        (setq results (nconc results
                             (company-cmake--parse prefix str cmd-opts)))))
    results))

(defun company-cmake--unexpand-candidate (candidate)
  (cond
   ((string-match "^CMAKE_\\(C\\|CXX\\|G77\\)\\(_.*\\)$" candidate)
    (setq candidate (concat "CMAKE_<LANG>_" (match-string 2 candidate))))

   ;; C flags
   ((string-match "^\\(.*_\\)IS_GNU\\(C\\|CXX\\|G77\\)$" candidate)
    (setq candidate (concat (match-string 1 candidate) "IS_GNU<LANG>")))

   ;; C flags
   ((string-match "^\\(.*_\\)OVERRIDE_\\(C\\|CXX\\|G77\\)$" candidate)
    (setq candidate (concat (match-string 1 candidate) "OVERRIDE_<LANG>")))

   ((string-match "^\\(.*\\)\\(_DEBUG\\|_RELEASE\\|_RELWITHDEBINFO\\|_MINSIZEREL\\)\\(.*\\)$" candidate)
    (setq candidate (concat (match-string 1 candidate)
                            "_<CONFIG>"
                            (match-string 3 candidate)))))
  candidate)

(defun company-cmake--meta (candidate)
  (let ((cmd-opts (gethash candidate company-cmake--meta-command-cache))
        result)
    (setq candidate (company-cmake--unexpand-candidate candidate))

    ;; Don't cache the documentation of every candidate (command)
    ;; Cache in this case will cost too much memory.
    (with-temp-buffer
      (call-process company-cmake-executable nil t nil cmd-opts candidate)
      ;; Go to the third line, trim it and return the result.
      ;; Tested with cmake 2.8.9.
      (goto-char (point-min))
      (forward-line 2)
      (setq result (buffer-substring-no-properties (line-beginning-position)
                                                   (line-end-position)))
      (setq result (replace-regexp-in-string "^[ \t\n\r]+" "" result))
      result)))

(defun company-cmake--doc-buffer (candidate)
  (let ((cmd-opts (gethash candidate company-cmake--meta-command-cache)))

    (setq candidate (company-cmake--unexpand-candidate candidate))
    (with-temp-buffer
      (call-process company-cmake-executable nil t nil cmd-opts candidate)
      ;; Go to the third line, trim it and return the doc buffer.
      ;; Tested with cmake 2.8.9.
      (goto-char (point-min))
      (forward-line 2)
      (company-doc-buffer
       (buffer-substring-no-properties (line-beginning-position)
                                       (point-max))))))

(defun company-cmake (command &optional arg &rest ignored)
  "`company-mode' completion back-end for CMake.
CMake is a cross-platform, open-source make system."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-cmake))
    (init (when (memq major-mode company-cmake-modes)
            (unless company-cmake-executable
              (error "Company found no cmake executable"))))
    (prefix (and (memq major-mode company-cmake-modes)
                 (not (company-in-string-or-comment))
                 (company-grab-symbol)))
    (candidates (company-cmake--candidates arg))
    (meta (company-cmake--meta arg))
    (doc-buffer (company-cmake--doc-buffer arg))
    ))

(provide 'company-cmake)
;;; company-cmake.el ends here
