;;; company-ropemacs.el --- a company-mode completion back-end for pysmell.el
;;
;; Copyright (C) 2009-2010 Nikolaj Schumacher
;;
;; This file is part of company 0.5.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(eval-when-compile (require 'cl))
(require 'pymacs)

(unless (fboundp 'rope-completions)
  (pymacs-load "ropemacs" "rope-"))

(unless (fboundp 'rope-completions)
  (error "rope-completions not found, try development version of ropemacs"))

(defun company-ropemacs--grab-symbol ()
  (let ((symbol (company-grab-symbol)))
    (when symbol
      (cons symbol
            (save-excursion
              (let ((pos (point)))
                (goto-char (- (point) (length symbol)))
                (while (eq (char-before) ?.)
                  (goto-char (1- (point)))
                  (skip-syntax-backward "w_"))
                (- pos (point))))))))

(defun company-ropemacs-doc-buffer (candidate)
  "Return buffer with docstring of CANDIDATE if it is available."
  (let ((doc (company-with-candidate-inserted candidate (rope-get-doc))))
    (when doc
      (with-current-buffer (company-doc-buffer)
        (insert doc)
        (current-buffer)))))

(defun company-ropemacs-location (candidate)
  "Return location of CANDIDATE in cons form (FILE . LINE) if it is available."
  (let ((location (company-with-candidate-inserted candidate
                    (rope-definition-location))))
    (when location
      (cons (elt location 0) (elt location 1)))))

(defun company-ropemacs (command &optional arg &rest ignored)
  "A `company-mode' completion back-end for ropemacs."
  (interactive (list 'interactive))
  (case command
    ('interactive (company-begin-backend 'company-ropemacs))
    ('prefix (and (derived-mode-p 'python-mode)
                  (not (company-in-string-or-comment))
                  (company-ropemacs--grab-symbol)))
    ('candidates (mapcar (lambda (element) (concat arg element))
                         (rope-completions)))
    ('doc-buffer (company-ropemacs-doc-buffer arg))
    ('location (company-ropemacs-location arg))))

(provide 'company-ropemacs)
;;; company-ropemacs.el ends here
