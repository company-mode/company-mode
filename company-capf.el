;;; company-capf.el --- company-mode completion-at-point-functions backend -*- lexical-binding: t -*-

;; Copyright (C) 2013-2024  Free Software Foundation, Inc.

;; Author: Stefan Monnier <monnier@iro.umontreal.ca>

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

;;; Commentary:
;;
;; The CAPF back-end provides a bridge to the standard
;; completion-at-point-functions facility, and thus can support any major mode
;; that defines a proper completion function, including emacs-lisp-mode,
;; css-mode and nxml-mode.

;;; Code:

(require 'company)
(require 'cl-lib)

(defgroup company-capf nil
  "Completion backend as adapter for `completion-at-point-functions'."
  :group 'company)

(defcustom company-capf-disabled-functions '(tags-completion-at-point-function
                                             ispell-completion-at-point)
  "List of completion functions which should be ignored in this backend.

By default it contains the functions that duplicate the built-in backends
but don't support the corresponding configuration options and/or alter the
intended priority of the default backends' configuration."
  :type 'hook
  :package-version '(company . "1.0.0"))

;; Amortizes several calls to a c-a-p-f from the same position.
(defvar company--capf-cache nil)

;; FIXME: Provide a way to save this info once in Company itself
;; (https://github.com/company-mode/company-mode/pull/845).
(defvar-local company-capf--current-completion-data nil
  "Value last returned by `company-capf' in response to `candidates'.
For most properties/actions, this is just what we need: the exact values
that accompanied the completion table that's currently is use.

`company-capf', however, could be called at some different positions during
a completion session (most importantly, by `company-sort-by-occurrence'),
so we can't just use the preceding variable instead.")

(defvar-local company-capf--current-completion-metadata nil
  "Metadata computed with the current prefix and data above.")

(defun company--capf-data ()
  (let ((cache company--capf-cache))
    (if (and (equal (current-buffer) (car cache))
             (equal (point) (car (setq cache (cdr cache))))
             (equal (buffer-chars-modified-tick) (car (setq cache (cdr cache)))))
        (cadr cache)
      (let ((data (company--capf-data-real)))
        (setq company--capf-cache
              (list (current-buffer) (point) (buffer-chars-modified-tick) data))
        data))))

(defun company--capf-data-real ()
  (let ((data (run-hook-wrapped 'completion-at-point-functions
                                ;; Ignore disabled and misbehaving functions.
                                #'company--capf-wrapper 'optimist)))
    (when (and (consp (cdr data)) (integer-or-marker-p (nth 1 data))) data)))

(defun company--capf-wrapper (fun which)
  ;; E.g. tags-completion-at-point-function subverts company-etags in the
  ;; default value of company-backends, where the latter comes later.
  (unless (memq fun company-capf-disabled-functions)
    (let ((buffer-read-only t)
          (inhibit-read-only nil)
          (completion-in-region-function
           (lambda (beg end coll pred)
             (throw 'company--illegal-completion-in-region
                    (list fun beg end coll :predicate pred)))))
      (catch 'company--illegal-completion-in-region
        (condition-case nil
            (completion--capf-wrapper fun which)
          (buffer-read-only nil))))))

(declare-function python-shell-get-process "python")

(defun company-capf--save-current-data (data metadata)
  (setq company-capf--current-completion-data data
        company-capf--current-completion-metadata metadata)
  (add-hook 'company-after-completion-hook
            #'company-capf--clear-current-data nil t))

(defun company-capf--clear-current-data (_ignored)
  (setq company-capf--current-completion-data nil
        company-capf--current-completion-metadata nil))

(defvar-local company-capf--sorted nil)
(defvar-local company-capf--current-boundaries nil)

(defun company-capf (command &optional arg &rest rest)
  "`company-mode' backend using `completion-at-point-functions'."
  (interactive (list 'interactive))
  (pcase command
    (`interactive (company-begin-backend 'company-capf))
    (`prefix
     (company-capf--prefix))
    (`candidates
     (company-capf--candidates arg (car rest)))
    (`sorted
     company-capf--sorted)
    (`match
     ;; Ask the for the `:company-match' function.  If that doesn't help,
     ;; fallback to sniffing for face changes to get a suitable value.
     (let ((f (or (plist-get (nthcdr 4 company-capf--current-completion-data)
                             :company-match)
                  #'company--match-from-capf-face)))
       (funcall f arg)))
    (`duplicates t)
    (`no-cache t)   ;Not much can be done here, as long as we handle
                    ;non-prefix matches.
    (`meta
     (let ((f (plist-get (nthcdr 4 company-capf--current-completion-data)
                         :company-docsig)))
       (when f (funcall f arg))))
    (`doc-buffer
     (let ((f (plist-get (nthcdr 4 company-capf--current-completion-data)
                         :company-doc-buffer)))
       (when f (funcall f arg))))
    (`location
     (let ((f (plist-get (nthcdr 4 company-capf--current-completion-data)
                         :company-location)))
       (when f (funcall f arg))))
    (`annotation
     (company-capf--annotation arg))
    (`kind
     (let ((f (plist-get (nthcdr 4 company-capf--current-completion-data)
                         :company-kind)))
       (when f (funcall f arg))))
    (`deprecated
     (let ((f (plist-get (nthcdr 4 company-capf--current-completion-data)
                         :company-deprecated)))
       (when f (funcall f arg))))
    (`require-match
     (plist-get (nthcdr 4 (company--capf-data)) :company-require-match))
    (`init nil)      ;Don't bother: plenty of other ways to initialize the code.
    (`post-completion
     (company-capf--post-completion arg))
    (`adjust-boundaries
     (company--capf-boundaries
      company-capf--current-boundaries))
    (`expand-common
     (company-capf--expand-common arg (car rest)))
    ))

(defun company-capf--prefix ()
  (let ((res (company--capf-data)))
    (when res
      (let ((length (plist-get (nthcdr 4 res) :company-prefix-length))
            (prefix (buffer-substring-no-properties (nth 1 res) (point)))
            (suffix (buffer-substring-no-properties (point) (nth 2 res))))
        (list prefix suffix length)))))

(defun company-capf--expand-common (prefix suffix)
  (let* ((data company-capf--current-completion-data)
         (table (nth 3 data))
         (pred (plist-get (nthcdr 4 data) :predicate)))
    (company--capf-expand-common prefix suffix table pred
                                 company-capf--current-completion-metadata)))

(defun company-capf--annotation (arg)
  (let* ((f (or (plist-get (nthcdr 4 company-capf--current-completion-data)
                           :annotation-function)
                ;; FIXME: Add a test.
                (cdr (assq 'annotation-function
                           company-capf--current-completion-metadata))))
         (annotation (when f (funcall f arg))))
    (if (and company-format-margin-function
             (equal annotation " <f>") ; elisp-completion-at-point, pre-icons
             (plist-get (nthcdr 4 company-capf--current-completion-data)
                        :company-kind))
        nil
      annotation)))

(defun company-capf--candidates (input suffix)
  (let* ((current-capf (car company-capf--current-completion-data))
         (res (company--capf-data))
         (table (nth 3 res))
         (pred (plist-get (nthcdr 4 res) :predicate))
         (meta (and res
                    (completion-metadata
                     (buffer-substring (nth 1 res) (nth 2 res))
                     table pred))))
    (when (and res
               (or (not current-capf)
                   (equal current-capf (car res))))
      (let* ((interrupt (plist-get (nthcdr 4 res) :company-use-while-no-input))
             (all-result (company-capf--candidates-1 input suffix
                                                     table pred
                                                     meta
                                                     (and non-essential
                                                          (eq interrupt t))))
             (sortfun (cdr (assq 'display-sort-function meta)))
             (candidates (assoc-default :completions all-result)))
        (setq company-capf--sorted (functionp sortfun))
        (when candidates
          (company-capf--save-current-data res meta)
          (setq company-capf--current-boundaries
                (company--capf-boundaries-markers
                 (assoc-default :boundaries all-result)
                 company-capf--current-boundaries)))
        (when sortfun
          (setq candidates (funcall sortfun candidates)))
        candidates))))

(defun company-capf--candidates-1 (prefix suffix table pred meta interrupt-on-input)
  (if (not interrupt-on-input)
      (company--capf-completions prefix suffix table pred meta)
    (let (res)
      (and (while-no-input
             (setq res
                   (company--capf-completions prefix suffix table pred meta))
             nil)
           (throw 'interrupted 'new-input))
      res)))

(defun company-capf--post-completion (arg)
  (let* ((res company-capf--current-completion-data)
         (exit-function (plist-get (nthcdr 4 res) :exit-function))
         (table (nth 3 res)))
    (if exit-function
        ;; Follow the example of `completion--done'.
        (funcall exit-function arg
                 ;; FIXME: Should probably use an additional heuristic:
                 ;; completion-at-point doesn't know when the user picked a
                 ;; particular candidate explicitly (it only checks whether
                 ;; further completions exist). Whereas company user can press
                 ;; RET (or use implicit completion with company-tng).
                 (if (= (car (completion-boundaries arg table nil ""))
                        (length arg))
                     'exact
                   'finished)))))

(provide 'company-capf)

;;; company-capf.el ends here
