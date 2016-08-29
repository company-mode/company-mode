;;; company-flow.el --- Flow backend for company-mode  -*- lexical-binding: t -*-

;; Copyright (C) 2016 by Aaron Jensen

;; Author: Aaron Jensen <aaronjensen@gmail.com>
;; URL: https://github.com/aaronjensen/company-flow
;; Version: 0.1.0
;; Package-Requires: ((company "0.8.0") (dash "2.13.0"))

;;; Commentary:

;; This package adds support for flow to company. It requires
;; flow to be in your path.

;; To use it, add to your company-backends for your preferred javascript modes,
;; for example:

;; (setq company-backends-js2-mode '((company-flow :with company-dabbrev)
;;                                     company-files
;;                                     company-dabbrev))

;;; License:

;; This file is not part of GNU Emacs.
;; However, it is distributed under the same license.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:
(require 'company)
(require 'dash)

(defun company-flow--handle-signal (process _event)
  (when (memq (process-status process) '(signal exit))
    (let ((callback (process-get process 'company-flow-callback))
          (prefix (process-get process 'company-flow-prefix)))
      ;; (message "%S" (company-flow--parse-output (company-flow--get-output process)))
      (funcall callback (->> process
                             company-flow--get-output
                             company-flow--parse-output
                             ;; Remove nils
                             (--filter it)
                             (all-completions prefix))))))

(defun company-flow--make-candidate (line)
  "Creates a candidate with a meta property from LINE.

LINE is expected to look like:
registrationSuccess () => {type: 'REGISTRATION_SUCCESS'}"
  (let ((first-space (string-match " " line)))
    (when first-space
      (let ((text (substring line 0 first-space))
            (meta (substring line (+ 1 first-space))))
        (propertize text 'meta meta)))))

(defun company-flow--parse-output (output)
  (mapcar 'company-flow--make-candidate
          (split-string output "\n")))

(defun company-flow--get-output (process)
  "Get the complete output of PROCESS."
  (with-demoted-errors "Error while retrieving process output: %S"
    (let ((pending-output (process-get process 'company-flow-pending-output)))
      (apply #'concat (nreverse pending-output)))))

(defun company-flow--receive-checker-output (process output)
  "Receive a syntax checking PROCESS OUTPUT."
  (push output (process-get process 'company-flow-pending-output)))

(defun company-flow--process-send-buffer (process)
  "Send all contents of current buffer to PROCESS.

Sends all contents of the current buffer to the standard input of
PROCESS, and terminates standard input with EOF."
  (save-restriction
    (widen)
    (process-send-region process (point-min) (point-max)))
  (process-send-eof process))

(defun company-flow--candidates-query (prefix callback)
  (let* ((line (line-number-at-pos (point)))
         (col (+ 1 (current-column)))
         (command (list "flow"
                        "autocomplete"
                        buffer-file-name
                        (number-to-string line)
                        (number-to-string col)))
         (process (apply 'start-process "company-flow" nil command)))
    (set-process-sentinel process #'company-flow--handle-signal)
    (set-process-filter process #'company-flow--receive-checker-output)
    (process-put process 'company-flow-callback callback)
    (process-put process 'company-flow-prefix prefix)
    (company-flow--process-send-buffer process)))

(defun company-flow--prefix ()
  "Grab prefix for flow."
  (and (not (company-in-string-or-comment))
       (or (company-grab-symbol-cons "\\." 1)
           'stop)))

(defun company-flow--annotation (candidate)
  (format " %s" (get-text-property 0 'meta candidate)))

(defun company-flow--meta (_candidate)
  nil)

(defun company-flow--doc (_candidate)
  nil)

;;;###autoload
(defun company-flow (command &optional arg &rest _args)
  (interactive (list 'interactive))
  (pcase command
    (`interactive (company-begin-backend 'company-flow))
    (`prefix (company-flow--prefix))
    (`annotation (company-flow--annotation arg))
    (`meta (company-flow--meta arg))
    (`doc-buffer (company-flow--doc arg))
    (`ignore-case t)
    (`sorted t)
    (`candidates (cons :async
                      (lambda (callback)
                        (company-flow--candidates-query arg callback))))))

(provide 'company-flow)
;;; company-flow.el ends here
