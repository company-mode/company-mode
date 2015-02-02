;;; company-kill-ring.el --- company-mode completion back-end for the kill-ring

;; Copyright (C) 2009-2011  Free Software Foundation, Inc.

;; Author: Nikolaj Schumacher

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

(require 'company)
(require 'cl-lib)

;;;###autoload
(defun company-kill-ring (command &optional arg &rest ignored)
  "`company-mode' completion for the kill-ring."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-kill-ring))
    (prefix "")
    (candidates (mapcar #'substring-no-properties kill-ring))))

;;;###autoload
(defun company-kill-ring-search ()
  "Open a `company-mode' popup with the `kill-ring' for search and selection."
  (interactive)
  (let ((company-backends '(company-kill-ring)))
    (company-manual-begin)
    (company-filter-candidates)))

(provide 'company-kill-ring)
;;; company-kill-ring.el ends here
