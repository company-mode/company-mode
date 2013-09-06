;;; company-capf.el --- company-mode completion-at-point-functions back-end -*- lexical-binding: t -*-

;; Copyright (C) 2013  Free Software Foundation, Inc.

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
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.


;;; Commentary:
;;

;;; Code:

(defun company--capf-data ()
  (let ((data (run-hook-wrapped 'completion-at-point-functions
                                ;; Ignore misbehaving functions.
                                #'completion--capf-wrapper 'optimist)))
    (when (consp data) data)))

(defun company-capf (command &optional arg &rest _args)
  "`company-mode' back-end using `completion-at-point-functions'.
Requires Emacs 24.1 or newer."
  (interactive (list 'interactive))
  (pcase command
    (`interactive (company-begin-backend 'company-capf))
    (`prefix
     (let ((res (company--capf-data)))
       (when res
         (if (> (nth 2 res) (point))
             'stop
           (buffer-substring-no-properties (nth 1 res) (point))))))
    (`candidates
     (let ((res (company--capf-data)))
       (when res
         (let* ((table (nth 3 res))
                (pred (plist-get (nthcdr 4 res) :predicate))
                (meta (completion-metadata
                      (buffer-substring (nth 1 res) (nth 2 res))
                      table pred))
                (sortfun (cdr (assq 'display-sort-function meta)))
                (candidates (all-completions arg table pred)))
           (if sortfun (funcall sortfun candidates) candidates)))))
    (`sorted
     (let ((res (company--capf-data)))
       (when res
         (let ((meta (completion-metadata
                      (buffer-substring (nth 1 res) (nth 2 res))
                      (nth 3 res) (plist-get (nthcdr 4 res) :predicate))))
           (cdr (assq 'display-sort-function meta))))))
    (`duplicates nil) ;Don't bother.
    (`no-cache t)     ;FIXME: Improve!
    (`meta
     (let ((f (plist-get (nthcdr 4 (company--capf-data)) :company-docsig)))
       (when f (funcall f arg))))
    (`doc-buffer
     (let ((f (plist-get (nthcdr 4 (company--capf-data)) :company-doc-buffer)))
       (when f (funcall f arg))))
    (`location
     (let ((f (plist-get (nthcdr 4 (company--capf-data)) :company-location)))
       (when f (funcall f arg))))
    (`require-match
     (plist-get (nthcdr 4 (company--capf-data)) :company-require-match))
    (`init nil)      ;Don't bother: plenty of other ways to initialize the code.
    (`post-completion
     (let* ((res (company--capf-data))
            (exit-function (plist-get (nthcdr 4 res) :exit-function)))
       (if exit-function
           (funcall exit-function arg 'finished))))
    ))

(provide 'company-capf)

;;; company-capf.el ends here
