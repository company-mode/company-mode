;;; company-cdlatex.el --- company-mode completion backend for cdlatex

;; Copyright (C) 2020  Free Software Foundation, Inc.

;; Author: Benedikt Tissot

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

(declare-function cdlatex-tab "cdlatex")
(declare-function org-inside-LaTeX-fragment-p "org")

;;;###autoload
(defun company-cdlatex-backend (command &optional arg &rest ignored)
    "`company-mode' backend for `cdlatex'.

Suggest cdlatex-commands and expand them using `cdlatex-tab'.
Works in `org-mode' latex fragments and `latex-mode'."
    (interactive (list 'interactive))
    (cl-case command
      (interactive (company-begin-backend 'company-cdlatex-backend))
      (prefix (if (and (or (org-inside-LaTeX-fragment-p)
                           (eq 'major-mode 'latex-mode)))
                  (let ((prefix
                         (if (looking-at "\\$\\|\\_>")
                             (buffer-substring (point) (save-excursion (skip-syntax-backward "w_")
                                                                       (point)))
                           (unless (and (char-after) (memq (char-syntax (char-after)) '(?w ?_)))
                             ""))))
                    ;; no cdlatex when string starts with \
                    (when (and prefix (not (string-prefix-p "\\" prefix)))
                      (string-remove-prefix "$" prefix)))))
      (candidates (cl-remove-if-not
                   (lambda (c) (string-prefix-p arg c))
                   (cl-map 'list #'car cdlatex-command-alist-comb)))
      (annotation (concat
                   (unless company-tooltip-align-annotations " -> ")
                   (nth 2 (assoc arg cdlatex-command-alist-comb))))
      (post-completion
       (cdlatex-tab))))

(provide 'company-cdlatex)
;;; company-cdlatex.el ends here
