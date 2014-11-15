;;; company-mu.el --- company-mode completion back-end for address via mu cfind
;;; in message-mode or mail-mode

;; Copyright (C) 2014 Erik Hetzner

;; Author: Erik Hetzner

;;; Commentary:

;; This company mode backend allows a user to complete email addresses in mail-
;; or message-mode using the mu cfind command.

;;; Code:

(require 'company)

(defun company-mu-cfind-candidates (query)
  "Return a list of completion candidates for QUERY by calling mu cfind."
  (let* ((contacts-raw
          (shell-command-to-string (format "mu cfind %s --format mutt-ab"
                                           (shell-quote-argument query))))
         (contacts-split (cdr (split-string contacts-raw
                                            "[\n\r]+" t split-string-default-separators)))
         retval)
    (if (string= (car contacts-split) "mu: no matching contacts found")
        nil
     (dolist (raw contacts-split retval)
       (let* ((s (split-string raw "\t"))
              (addr (car s))
              (nm (car (cdr s))))
         (if (or (null nm)
                 (and (not (null nm))
                      (not (null addr))
                      (string= (downcase nm) (downcase addr))))
             ;; just add addr
             (setq retval (cons addr retval))
           ;; add addr & Name <addr>
           (setq retval (cons (format "%s <%s>" nm addr)
                              retval))))))))

(defun company-mu (command &optional arg &rest ignored)
  "`company-mode' completion back-end for mu'."
  (interactive (list 'interactive))
  (pcase command
    (`interactive (company-begin-backend 'company-mu))
    (`prefix (and (derived-mode-p 'mail-mode 'message-mode)
                  (save-excursion
                    (let ((end (point)))
                      (beginning-of-line)
                      (if (not (looking-at "^\\(To\\|Cc\\|Bcc\\): *\\(.*\\)"))
                          nil
                        (goto-char end)
                        (re-search-backward "\\(\\`\\|[\n:,]\\)[ \t]*")
                        (goto-char (match-end 0))
                        (buffer-substring-no-properties (point) end))))))
    (`candidates (company-mu-cfind-candidates arg))
    (`no-cache t)
    (`match (string-match (regexp-quote company-prefix)
                          arg)
            (match-end 0))))

(provide 'company-mu)
;;; company-mu.el ends here
