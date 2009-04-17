;;; company-dabbrev-code.el --- a dabbrev-like company-mode back-end for code
;;
;; Copyright (C) 2009 Nikolaj Schumacher
;;
;; This file is part of company 0.3.1.
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
(eval-when-compile (require 'cl))

(defcustom company-dabbrev-code-modes
  '(asm-mode batch-file-mode c++-mode c-mode cperl-mode csharp-mode css-mode
    emacs-lisp-mode erlang-mode espresso-mode f90-mode fortran-mode
    haskell-mode java-mode javascript-mode jde-mode js2-mode lisp-mode
    lua-mode objc-mode perl-mode php-mode python-mode ruby-mode scheme-mode
    shell-script-mode)
  "*Modes that use `company-dabbrev-code'.
In all these modes `company-dabbrev-code' will complete only symbols, not text
in comments or strings.  In other modes `company-dabbrev-code' will pass control
to other back-ends \(e.g. `company-dabbrev'\).
Value t means complete in all modes."
  :group 'company
  :type '(choice (repeat (symbol :tag "Major mode"))
                 (const tag "All modes" t)))

(defcustom company-dabbrev-code-other-buffers t
  "*Determines whether `company-dabbrev-code' should search other buffers.
If t, search all buffers with the same major-mode.
See also `company-dabbrev-code-time-limit'."
  :group 'company
  :type '(choice (const :tag "Off" nil)
                 (const :tag "Same major mode" t)))

(defcustom company-dabbrev-code-time-limit .5
  "*Determines how long `company-dabbrev-code' should look for matches."
  :group 'company
  :type '(choice (const :tag "Off" nil)
                 (number :tag "Seconds")))

(defmacro company-dabrev-code--time-limit-while (test start limit &rest body)
  (declare (indent 3) (debug t))
  `(let ((company-time-limit-while-counter 0))
     (catch 'done
       (while ,test
         ,@body
         (and ,limit
              (eq (incf company-time-limit-while-counter) 25)
              (setq company-time-limit-while-counter 0)
              (> (float-time (time-since ,start)) ,limit)
              (throw 'done 'company-time-out))))))

(defsubst company-dabbrev-code--make-regexp (prefix)
  (concat "\\_<" (if (equal prefix "")
                     "\\([a-zA-Z]\\|\\s_\\)"
                   (regexp-quote prefix))
          "\\(\\sw\\|\\s_\\)*\\_>"))

(defun company-dabbrev-code--buffer-symbols (regexp pos &optional symbols
                                             start limit)
  (save-excursion
    (let (match)
      (goto-char (if pos (1- pos) (point-min)))
      ;; search before pos
      (company-dabrev-code--time-limit-while (re-search-backward regexp nil t)
          start limit
        (setq match (match-string-no-properties 0))
        (if (company-in-string-or-comment)
            (re-search-backward "\\s<\\|\\s!\\|\\s\"\\|\\s|" nil t)
          (push match symbols)))
      (goto-char (or pos (point-min)))
      ;; search after pos
      (company-dabrev-code--time-limit-while (re-search-forward regexp nil t)
          start limit
        (setq match (match-string-no-properties 0))
        (if (company-in-string-or-comment)
            (re-search-forward "\\s>\\|\\s!\\|\\s\"" nil t)
          (push match symbols)))
      symbols)))

(defun company-dabbrev-code--symbols (regexp)
  (let* ((start (current-time))
         (limit company-dabbrev-code-time-limit)
         (symbols (company-dabbrev-code--buffer-symbols regexp (point) nil
                                                        start limit)))
    (when company-dabbrev-code-other-buffers
      (dolist (buffer (delq (current-buffer) (buffer-list)))
        (and (eq (buffer-local-value 'major-mode buffer) major-mode)
             (with-current-buffer buffer
               (setq symbols
                     (company-dabbrev-code--buffer-symbols regexp nil symbols
                                                           start limit))))
        (and limit
             (> (float-time (time-since start)) limit)
             (return))))
    symbols))

;;;###autoload
(defun company-dabbrev-code (command &optional arg &rest ignored)
  "A dabbrev-like `company-mode' back-end for code.
The back-end looks for all symbols in the current buffer that aren't in
comments or strings."
  (interactive (list 'interactive))
  (case command
    ('interactive (company-begin-backend 'company-dabbrev-code))
    ('prefix (and (or (eq t company-dabbrev-code-modes)
                      (apply 'derived-mode-p company-dabbrev-code-modes))
                  (not (company-in-string-or-comment))
                  (or (company-grab-symbol) 'stop)))
    ('candidates (let ((case-fold-search nil))
                   (company-dabbrev-code--symbols
                    (company-dabbrev-code--make-regexp arg))))
    ('duplicates t)))

(provide 'company-dabbrev-code)
;;; company-dabbrev-code.el ends here
