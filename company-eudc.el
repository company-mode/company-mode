;;; company-eudc.el --- company-mode completion backend for EUDC  -*- lexical-binding: t -*-

;; Copyright (c) 2020 condition-alpha.com

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;    This library provides an interface to EUDC. EUDC is the Emacs
;;    Unified Directory Client, a common interface to directory
;;    servers and contact information, and part of core Emacs. EUDC
;;    can consult mutiple servers for each query, providing the
;;    combined results through a single interface. EUDC has backends
;;    for LDAP servers, and to the BBDB.

;;; Usage:

;;    See `company-eudc', `company-eudc-activate-autocomplete', and
;;    `company-eudc-expand-inline'.

;;; Code:

;;{{{      Internal cooking

(require 'company)
(require 'cl-lib)

(defun company-eudc-email-address-from-alist (rec)
  "Generate an email address string (\"first last <email>\") from
  an attribute list."
  (let* ((name (cdr (assoc 'name rec)))
	 (firstname (cdr (assoc 'firstname rec)))
	 (email (cdr (assoc 'email rec)))
	 (candidate ""))
    (if firstname
	(setq candidate (format "%s %s <%s>" firstname name email))
      (setq candidate (format "%s <%s>" name email)))
    candidate))

;;}}}

;;{{{      High-level interfaces

;;;###autoload
(defun company-eudc (command &optional arg &rest ignored)
  "`company-mode' completion backend for EUDC.
EUDC is the Emacs Unified Directory Client, a common interface to
directory servers and contact information.

Completion will be attempted in the To, Cc, Bcc, From, and
Reply-To header fields of message buffers only. I.e. you can not
use this backend for completing email addresses outside a message
header.

Without additonal configuration, no completions from EUDC will be offered
yet. You will additionally need to decide how you want to use it. Since
EUDC can query remote resources (such as e.g. LDAP servers), the completion
process may take a while. Depending on how you have configured
`company-mode', this may result in Emacs being blocked for extensive
periods of time. To accommodate this, the `company-eudc' backend can either
install itself into the list of `company-mode' backends for
`message-mode' (see `company-eudc-activate-autocomplete'), or provide
autocompletion through `company-mode' only when bound to a key (see
`company-eudc-expand-inline')."
  (interactive (list 'interactive))
  (pcase command
    (`interactive (company-begin-backend 'company-eudc))
    (`prefix (and (derived-mode-p 'message-mode)
		  (let ((case-fold-search t))
		    (looking-back
		     "^\\([^ :]*-\\)?\\(To\\|B?Cc\\|From\\|Reply-to\\):.*? *\\([^,;]*\\)"
		     (line-beginning-position)))
		  (company-grab-symbol)))
    (`candidates (let* ((q-result (eudc-query `((name . ,arg)))))
		   (cl-loop for person-record in q-result
			    collect (company-eudc-email-address-from-alist
				     person-record))
		  ))
    (`sorted t)))

;;;###autoload
(defun company-eudc-activate-autocomplete ()
  "Provide `company-eudc' completions under `company-mode' control.
This function installs `company-eudc' in the list of
`company-mode' backends for `message-mode'. Completion candidates
from EUDC will thus be offered by `company-mode' as any other
candidate.

To get this behaviour, do

    (company-eudc-activate-autocomplete)

If you have configured many and/or slow servers for EUDC, this
will block Emacs for some time (i.e. until EUDC has delivered its
results). If this is a frequent issue, and you would like to
avoid this, use `company-eudc-expand-inline' instead. "
  (interactive)
  (add-to-list 'company-backends 'company-eudc)
  (add-hook 'message-mode-hook
	    (lambda () (add-to-list 'company-backends 'company-eudc))))

;;;###autoload
(defun company-eudc-expand-inline ()
  "Provide `company-eudc' completions in `message-mode' interactively.
This function triggers `company-mode' completion at point, using
the `company-eudc' backend only. It is intended for being bound
to a key chord; for example:

    (with-eval-after-load \"message\"
      (define-key message-mode-map (kbd \"<C-tab>\") 'company-eudc-expand-inline))

This may be advantageous if you have configured many and/or slow
servers for EUDC. By deferring potentialliy lengthy EUDC queries
to a specific key chord, the waiting time for the EUDC results to
arrive will be incurred on your explicit request only, and will
not seemingly block the otherwise speedy `company-mode' user
interface.

The advantage of binding `company-eudc-expand-inline' to a key,
instead of using `eudc-expand-inline' directly, is that
`company-eudc-expand-inline' uses the `company-mode' user
interface, whereas `eudc-expand-inline' provides its own user
interface, and which is different from `company-mode'."
  (interactive)
  (company-begin-backend 'company-eudc))

;;}}}

(provide 'company-eudc)

;;; company-eudc.el ends here
