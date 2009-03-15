;;; company-dabbrev.el --- a company-mode completion back-end for dabbrev
;;
;; Copyright (C) 2009 Nikolaj Schumacher
;;
;; This file is part of company.
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

(require 'company)
(require 'dabbrev)
(eval-when-compile (require 'cl))

(defun company-grab-dabbrev-prefix ()
  (save-excursion
    (when (looking-at "\\>")
      (let ((end (point)))
        (dabbrev--reset-global-variables)
        (dabbrev--goto-start-of-abbrev)
        (buffer-substring-no-properties (point) end)))))

(defun company-dabbrev (command &optional arg &rest ignored)
  (case command
    ('prefix (company-grab-dabbrev-prefix))
    ('candidates (let ((dabbrev-check-other-buffers))
                   (dabbrev--reset-global-variables)
                   (dabbrev--find-all-expansions arg t)))
    ('ignore-case t)))

(provide 'company-dabbrev)
;;; company-dabbrev.el ends here
