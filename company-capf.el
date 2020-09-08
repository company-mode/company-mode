;;; company-capf.el --- company-mode completion-at-point-functions backend -*- lexical-binding: t -*-

;; Copyright (C) 2013-2019  Free Software Foundation, Inc.

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

;; Amortizes several calls to a c-a-p-f from the same position.
(defvar company--capf-cache nil)

;; FIXME: Provide a way to save this info once in Company itself
;; (https://github.com/company-mode/company-mode/pull/845).
(defvar-local company-capf--current-completion-data nil
  "Value last returned by `company-capf' when called with `candidates'.
For most properties/actions, this is just what we need: the exact values
that accompanied the completion table that's currently is use.

`company-capf', however, could be called at some different positions during
a completion session (most importantly, by `company-sort-by-occurrence'),
so we can't just use the preceding variable instead.")

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
  (cl-letf* (((default-value 'completion-at-point-functions)
              ;; Ignore tags-completion-at-point-function because it subverts
              ;; company-etags in the default value of company-backends, where
              ;; the latter comes later.
              (remove 'tags-completion-at-point-function
                      (default-value 'completion-at-point-functions)))
             (completion-at-point-functions (company--capf-workaround))
             (data (run-hook-wrapped 'completion-at-point-functions
                                     ;; Ignore misbehaving functions.
                                     #'completion--capf-wrapper 'optimist)))
    (when (and (consp (cdr data)) (integer-or-marker-p (nth 1 data))) data)))

(declare-function python-shell-get-process "python")

(defun company--capf-workaround ()
  ;; For http://debbugs.gnu.org/cgi/bugreport.cgi?bug=18067
  (if (or (not (listp completion-at-point-functions))
          (not (memq 'python-completion-complete-at-point completion-at-point-functions))
          (python-shell-get-process))
      completion-at-point-functions
    (remq 'python-completion-complete-at-point completion-at-point-functions)))

(defun company-capf--save-current-data (data)
  (setq company-capf--current-completion-data data)
  (add-hook 'company-after-completion-hook
            #'company-capf--clear-current-data nil t))

(defun company-capf--clear-current-data (_ignored)
  (setq company-capf--current-completion-data nil))

(defvar-local company-capf--sorted nil)

(defun company-capf (command &optional arg &rest _args)
  "`company-mode' backend using `completion-at-point-functions'."
  (interactive (list 'interactive))
  (pcase command
    (`interactive (company-begin-backend 'company-capf))
    (`prefix
     (let ((res (company--capf-data)))
       (when res
         (let ((length (plist-get (nthcdr 4 res) :company-prefix-length))
               (prefix (buffer-substring-no-properties (nth 1 res) (point))))
           (cond
            ((> (nth 2 res) (point)) 'stop)
            (length (cons prefix length))
            (t prefix))))))
    (`candidates
     (company-capf--candidates arg))
    (`sorted
     company-capf--sorted)
    (`match
     ;; Ask the for the `:company-match' function.  If that doesn't help,
     ;; fallback to sniffing for face changes to get a suitable value.
     (let ((f (plist-get (nthcdr 4 company-capf--current-completion-data)
                         :company-match)))
       (if f (funcall f arg)
         (let* ((match-start nil) (pos -1)
                (prop-value nil)  (faces nil)
                (has-face-p nil)  chunks
                (limit (length arg)))
           (while (< pos limit)
             (setq pos
                   (if (< pos 0) 0 (next-property-change pos arg limit)))
             (setq prop-value (or
                               (get-text-property pos 'face arg)
                               (get-text-property pos 'font-lock-face arg))
                   faces (if (listp prop-value) prop-value (list prop-value))
                   has-face-p (memq 'completions-common-part faces))
             (cond ((and (not match-start) has-face-p)
                    (setq match-start pos))
                   ((and match-start (not has-face-p))
                    (push (cons match-start pos) chunks)
                    (setq match-start nil))))
           (nreverse chunks)))))
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
     (let ((f (plist-get (nthcdr 4 company-capf--current-completion-data)
                         :annotation-function)))
       (when f (funcall f arg))))
    (`require-match
     (plist-get (nthcdr 4 (company--capf-data)) :company-require-match))
    (`init nil)      ;Don't bother: plenty of other ways to initialize the code.
    (`post-completion
     (company--capf-post-completion arg))
    ))

(defun company-capf--candidates (input)
  (let ((res (company--capf-data)))
    (company-capf--save-current-data res)
    (when res
      (let* ((table (nth 3 res))
             (pred (plist-get (nthcdr 4 res) :predicate))
             (meta (completion-metadata
                    (buffer-substring (nth 1 res) (nth 2 res))
                    table pred))
             (candidates (completion-all-completions input table pred
                                                     (length input)
                                                     meta))
             (sortfun (cdr (assq 'display-sort-function meta)))
             (last (last candidates))
             (base-size (and (numberp (cdr last)) (cdr last))))
        (when base-size
          (setcdr last nil))
        (setq company-capf--sorted (functionp sortfun))
        (when sortfun
          (setq candidates (funcall sortfun candidates)))
        (if (not (zerop (or base-size 0)))
            (let ((before (substring input 0 base-size)))
              (mapcar (lambda (candidate)
                        (concat before candidate))
                      candidates))
          candidates)))))

(defun company--capf-post-completion (arg)
  (let* ((res company-capf--current-completion-data)
         (exit-function (plist-get (nthcdr 4 res) :exit-function))
         (table (nth 3 res)))
    (if exit-function
        ;; We can more or less know when the user is done with completion,
        ;; so we do something different than `completion--done'.
        (funcall exit-function arg
                 ;; FIXME: Should probably use an additional heuristic:
                 ;; completion-at-point doesn't know when the user picked a
                 ;; particular candidate explicitly (it only checks whether
                 ;; further completions exist). Whereas company user can press
                 ;; RET (or use implicit completion with company-tng).
                 (if (= (car (completion-boundaries arg table nil ""))
                        (length arg))
                     'sole
                   'finished)))))

(provide 'company-capf)

;;; company-capf.el ends here
