;;; company-statistics.el --- history scoring using company-transformers

;; Copyright (C) 2014  Free Software Foundation, Inc.

;; Author: Ingo Lohmar

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
;; - backends decide on available candidates --- depends on prefix
;; - we store how often a candidate is chosen --- independent of prefixes
;; - for sorted candidates: stable sort keeps incoming order if same/no score
;; - TODO add ert tests
;; - TODO how to treat case, use backend's ignore-case?
;; - TODO maybe later depend on the mode, file, project: all in score functions

;;; Code:

(require 'company)

(defgroup company-statistics nil
  "Completion candidates ranking by historical statistics."
  :group 'company)

(defcustom company-statistics-size 400
  "Number of completion choices that `company-statistics' keeps track of.
As this is a global cache, making it too small defeats the purpose."
  :group 'company-statistics
  :type 'integer
  :initialize (lambda (option init-size) (setq company-statistics-size init-size))
  :set 'company-statistics--history-resize)

(defcustom company-statistics-file
  (concat user-emacs-directory "company-statistics-cache.el")
  "File to save company-statistics state."
  :group 'company-statistics
  :type 'string)

(defcustom company-statistics-auto-save t
  "Whether to save the statistics when leaving emacs."
  :group 'company-statistics
  :type 'boolean)

(defcustom company-statistics-auto-restore t
  "Whether to restore statistics when company-statistics is enabled and has
not been used before."
  :group 'company-statistics
  :type 'boolean)

;; internal vars, persistence

(defvar company-statistics--scores nil
  "Store selection frequency of candidates.")

(defvar company-statistics--history nil
  "Ring keeping the history of chosen candidates.")

(defvar company-statistics--history-replace nil
  "Index into the completion history.")

(defun company-statistics--init ()
  "Initialize company-statistics."
  (setq company-statistics--scores
        (make-hash-table :test 'equal :size company-statistics-size))
  (setq company-statistics--history (make-vector company-statistics-size nil)
        company-statistics--history-replace 0))

(defun company-statistics--initialized-p ()
  (hash-table-p company-statistics--scores))

(defun company-statistics--history-resize (option new-size)
  (when (company-statistics--initialized-p)
    ;; hash scoresheet auto-resizes, but history does not
    (let ((new-hist (make-vector new-size nil))
          ;; use actual length, to also work for freshly restored history
          (company-statistics-size (length company-statistics--history)))
      ;; copy newest entries (possibly nil) to new-hist
      (dolist (i (number-sequence 0 (1- (min new-size company-statistics-size))))
        (let ((old-i (mod (+ (- company-statistics--history-replace new-size) i)
                          company-statistics-size)))
          (aset new-hist i (aref company-statistics--history old-i))))
      ;; remove discarded history (when shrinking) from scores
      (when (< new-size company-statistics-size)
        (dolist (i (number-sequence
                    company-statistics--history-replace
                    (+ company-statistics-size
                       company-statistics--history-replace
                       (1- new-size))))
          (company-statistics--score-down
           (aref company-statistics--history (mod i company-statistics-size)))))
      (setq company-statistics--history new-hist)
      (setq company-statistics--history-replace (if (<= new-size company-statistics-size)
                                                    0
                                                  company-statistics-size))))
  (setq company-statistics-size new-size))

(defun company-statistics--save ()
  "Save statistics."
  (with-temp-buffer
    (let (print-level print-length)
      (insert
       (format
        "%S"
        `(setq
          company-statistics--scores ,company-statistics--scores
          company-statistics--history ,company-statistics--history
          company-statistics--history-replace ,company-statistics--history-replace))))
    (write-file company-statistics-file)))

(defun company-statistics--maybe-save ()
  (when company-statistics-auto-save
    (company-statistics--save)))

(defun company-statistics--load ()
  "Restore statistics."
  (load company-statistics-file 'noerror nil 'nosuffix))

;; score manipulation in one place

(defun company-statistics--score-get (cand)
  (gethash cand company-statistics--scores 0))

(defun company-statistics--score-up (cand)
  (puthash cand
           (1+ (company-statistics--score-get cand))
           company-statistics--scores))

(defun company-statistics--score-down (cand)
  (when cand                            ;ignore nil
    (let ((old-score (company-statistics--score-get cand)))
      ;; on scoresheet, decrease corresponding score or remove entry
      (if (> old-score 1)
          (puthash cand (1- old-score) company-statistics--scores)
        (remhash cand company-statistics--scores)))))

;; core functions: actual sorting transformer, statistics updater

(defun company-sort-by-statistics (candidates)
  "Sort candidates by historical statistics."
  (setq candidates
        (sort candidates
              (lambda (cand1 cand2)
                (>  (company-statistics--score-get cand1)
                    (company-statistics--score-get cand2))))))

(defun company-statistics--finished (result)
  "After completion, update scores and history."
  (setq result (substring-no-properties result)) ;on the safe side
  (company-statistics--score-up result)
  ;; update cyclic completion history
  (let ((replace-result
         (aref company-statistics--history company-statistics--history-replace)))
    (company-statistics--score-down replace-result)) ;void if nil
  ;; insert new result
  (aset company-statistics--history company-statistics--history-replace result)
  (setq company-statistics--history-replace
        (mod (1+ company-statistics--history-replace) company-statistics-size)))

;;;###autoload
(define-minor-mode company-statistics-mode
  "Statistical sorting for company-mode.  Ranks completion candidates by
the frequency with which they have been chosen in recent (as given by
`company-statistics-size') history.

Turning this mode on and off preserves the statistics.  They are also
preserved automatically between Emacs sessions in the default
configuration.  You can customize this behavior with
`company-statistics-auto-save', `company-statistics-auto-restore' and
`company-statistics-file'."
  nil nil nil
  :global t
  (if company-statistics-mode
      (progn
        (unless (company-statistics--initialized-p)
          (if company-statistics-auto-restore
              (progn
                (company-statistics--load) ;maybe of different size
                (company-statistics--history-resize nil company-statistics-size))
            (company-statistics--init)))
        (add-to-list 'company-transformers
                     'company-sort-by-statistics 'append)
        (add-hook 'company-completion-finished-hook
                  'company-statistics--finished))
    (setq company-transformers
          (delq 'company-sort-by-statistics company-transformers))
    (remove-hook 'company-completion-finished-hook
                 'company-statistics--finished)))

(add-hook 'kill-emacs-hook 'company-statistics--maybe-save)

(provide 'company-statistics)
;;; company-statistics.el ends here
