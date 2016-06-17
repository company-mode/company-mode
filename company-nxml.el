;;; company-nxml.el --- company-mode completion backend for nxml-mode

;; Copyright (C) 2009-2011, 2013-2016 Free Software Foundation, Inc.

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
;;  Changes by gitplass@arcor.de (Thomas Plass) 7-Jun-2016
;;
;;  With a RELAX NG schema loaded for the current buffer completion is more 
;;  context sensitive and schema aware.
;;
;;  When in tag name context (after a '<'), requesting completion will
;;   * auto-insert the next required element or default to presenting the candidate 
;;     list of optional elements along with the end tag for the current element
;;   * auto-insert all required attributes
;;   * prepare the next completion request to either
;;     - auto-insert the sole optional attribute or 
;;     - present the candidate list of optional attribute names. 
;;     This requires that the last completion action inserted a tag name but does
;;     not require a space to be manually inserted after the tag name.
;;
;;  Attributes are always inserted as attname="" with point positioned after the first quote.
;;
;;  Attribute completion is available at all legal positions in open as well as already
;;  closed tags.
;;
;;; Code:

(require 'company)
(require 'cl-lib)

(defvar rng-open-elements)
(defvar rng-validate-mode)
(defvar rng-in-attribute-regex)
(defvar rng-in-attribute-value-regex)
(declare-function rng-set-state-after "rng-nxml")
(declare-function rng-match-possible-start-tag-names "rng-match")
(declare-function rng-match-required-element-name "rng-match")
(declare-function rng-adjust-state-for-attribute "rng-nxml")
(declare-function rng-match-possible-attribute-names "rng-match")
(declare-function rng-match-required-attribute-names "rng-match")
(declare-function rng-adjust-state-for-attribute-value "rng-nxml")
(declare-function rng-match-possible-value-strings "rng-match")

(defconst company-nxml-token-regexp
  "\\(?:[_[:alpha:]][-._[:alnum:]]*\\)")

(defvar company-nxml-in-attribute-value-regexp
  (replace-regexp-in-string "w" company-nxml-token-regexp
   "<w\\(?::w\\)?\
\\(?:[ \t\r\n]+w\\(?::w\\)?[ \t\r\n]*=\
[ \t\r\n]*\\(?:\"[^\"]*\"\\|'[^']*'\\)\\)*\
[ \t\r\n]+\\(w\\(:w\\)?\\)[ \t\r\n]*=[ \t\r\n]*\
\\(\"\\([^\"]*\\)\\|'\\([^']*\\)\\)"
   t t))

(defvar company-nxml-in-tag-name-regexp
  (replace-regexp-in-string "w" company-nxml-token-regexp
                            "<\\(/?w\\(?::w?\\)?\\)?\\=" t t))

(defvar company-nxml-in-starttag-name-regexp
  (replace-regexp-in-string "w" company-nxml-token-regexp
                            "<w\\(?::w?\\)?\\="
                             t t))

(defvar-local company-nxml-last-command nil
  "Symbol that records the name of the function that most recently compiled 
the list of completion candidates.  This name drives 'post-completion.")

(defvar company-nxml-sort-attributes-by-length nil
  "When set to a non-nil value, attribute names will be sorted by length. 
This is based on the premise that a short name indicates significance.")

(defun company-nxml-all-completions (prefix alist)
  (let ((candidates (mapcar 'cdr alist))
        (case-fold-search nil)
        filtered)
    (when (cdar rng-open-elements)
      (push (concat "/" (cdar rng-open-elements)) candidates))
    (setq candidates (sort (all-completions prefix candidates) 'string<))
    (while candidates
      (unless (equal (car candidates) (car filtered))
        (push (car candidates) filtered))
      (pop candidates))
    (nreverse filtered)))

(defmacro company-nxml-prepared (&rest body)
  (declare (indent 0) (debug t))
  `(let ((lt-pos (save-excursion (search-backward "<" nil t)))
         xmltok-dtd)
     (when (and lt-pos (= (rng-set-state-after lt-pos) lt-pos))
       ,@body)))

(defun company-nxml-tag (command &optional arg &rest ignored)
  (cl-case command
    (prefix (and (derived-mode-p 'nxml-mode)
                 rng-validate-mode
                 (company-grab company-nxml-in-tag-name-regexp 1)))
    (candidates (let* ((required (company-nxml-prepared (rng-match-required-element-name)))
                       (candidates (if required
                                       (list (cdr required))
                                     (company-nxml-prepared
                                       (company-nxml-all-completions
                                        arg (rng-match-possible-start-tag-names))))))
                  (when candidates
                    (setq company-nxml-last-command 'company-nxml-tag)
                    candidates)))
    (sorted t)))

(defun company-nxml-attribute (command &optional arg &rest ignored)
  (cl-case command
    (prefix (and (derived-mode-p 'nxml-mode)
                 rng-validate-mode
                 (or (memq (char-before) '(?\  ?\t ?\n))     ; after tag name
                     (memq (char-after) '(?\  ?\t ?\n ?\>))) ; after attribute, before tagc 
                 (company-grab rng-in-attribute-regex 1)))
    (candidates (let* ((name (or arg ""))
                       (candidates (company-nxml-prepared
                                    (and (rng-adjust-state-for-attribute
                                          lt-pos (- (point) (length name)))
                                         (company-nxml-attribute-completions
                                          name (rng-match-possible-attribute-names))))))
                  (when candidates
                    (if arg
                        (setq company-nxml-last-command 'company-nxml-attribute)
                      (setq company-nxml-last-command 'company-nxml-attribute-from-tag
                            company-prefix "")) ; prevent tag name from acting as a prefix

                    candidates)))
    (sorted t)))


(defun company-nxml-add-required-attributes (tagname pos)
  "Add all required attributes as empty strings and position point in first attribute value.
Called post completion when inserting a tag name."
  (insert " ")
  (let ((atts (company-nxml-prepared
                (and (rng-adjust-state-for-attribute lt-pos (point))
                     (rng-match-required-attribute-names)))))
    (if (not atts)
        (delete-region pos (point)) ; clean up inserted space 
      (let ((attspecs (mapconcat
                       (lambda (att)
                         (concat (cdr att) "=\"\""))
                       atts
                       " ")))
        (insert attspecs)
        (goto-char pos)
        (search-forward "=\"\"")
        (forward-char -1)
        (message "Auto-inserted %srequired attribute%s for element %s"
                 (or (and (cdr atts) (format "%d " (length atts))) "")
                 (or (and (cdr atts) "s") (concat " " (cdr (car atts))))
                 tagname)))))


(defun company-nxml-attribute-completions (prefix alist)
  (let ((candidates (mapcar 'cdr alist))
         filtered)
    (when candidates
      (setq candidates
            (sort (all-completions prefix candidates)
                  (lambda (a b)
                    (if company-nxml-sort-attributes-by-length
                        (<= (length a) (length b))
                      (string< a b))))))
    (while candidates
      (unless (equal (car candidates) (car filtered))
        (push (car candidates) filtered))
      (pop candidates))
    (nreverse filtered)))


(defun company-nxml-attribute-value (command &optional arg &rest ignored)
  (cl-case command
    (prefix (and (derived-mode-p 'nxml-mode)
                 rng-validate-mode
                 (and (memq (char-after) '(?' ?\" ?\  ?\t ?\n)) ;; outside word
                      (looking-back company-nxml-in-attribute-value-regexp nil)
                      (or (match-string-no-properties 4)
                          (match-string-no-properties 5)
                          ""))))
    (candidates (company-nxml-prepared
                 (let (attr-start attr-end colon)
                   (and (looking-back rng-in-attribute-value-regex lt-pos)
                        (setq colon (match-beginning 2)
                              attr-start (match-beginning 1)
                              attr-end (match-end 1))
                        (rng-adjust-state-for-attribute lt-pos attr-start)
                        (rng-adjust-state-for-attribute-value
                         attr-start colon attr-end)
                        (setq company-nxml-last-command 'company-nxml-attribute-value)
                        (all-completions
                         arg (rng-match-possible-value-strings))))))))

;;;###autoload
(defun company-nxml (command &optional arg &rest ignored)
  "`company-mode' completion backend for `nxml-mode'."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-nxml))
    (prefix (or (company-nxml-tag 'prefix)
                (company-nxml-attribute 'prefix)
                (company-nxml-attribute-value 'prefix)))
    (candidates (cond
                 ((and (eq company-nxml-last-command 'company-nxml-tag)
                       (company-grab company-nxml-in-starttag-name-regexp 1 1))
                  (or (company-nxml-attribute 'candidates)
                      (company-nxml-tag 'candidates arg)))
                 ((company-nxml-tag 'prefix)
                  (company-nxml-tag 'candidates arg))
                 ((company-nxml-attribute 'prefix)
                  (company-nxml-attribute 'candidates arg))
                 ((company-nxml-attribute-value 'prefix)
                  (sort (company-nxml-attribute-value 'candidates arg)
                        'string<))))
    (post-completion (cond
                      ((eq company-nxml-last-command 'company-nxml-tag)
                       (company-nxml-add-required-attributes arg (point)))
                      ((eq company-nxml-last-command 'company-nxml-attribute-from-tag)
                       (backward-char (length arg))
                       (insert " ")
                       (forward-char (length arg))
                       (insert "=\"\"")
                       (backward-char 1))
                      ((eq company-nxml-last-command 'company-nxml-attribute)
                       (insert "=\"\"")
                       (backward-char 1))))
    (sorted t)))

(provide 'company-nxml)
;;; company-nxml.el ends here
