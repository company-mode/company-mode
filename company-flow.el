;;; company-flow.el --- Flow backend for company-mode  -*- lexical-binding: t -*-
(require 'cl-lib)

(defun company-flow-handle-signal (process _event)
  (when (memq (process-status process) '(signal exit))
    (let ((callback (process-get process 'company-flow-callback))
          (prefix (process-get process 'company-flow-prefix)))
      ;; (message "%S" (company-flow-parse-output (company-flow-get-output process)))
      (funcall callback (->> process
                             company-flow-get-output
                             company-flow-parse-output
                             (all-completions prefix))))))

(defun company-flow-make-candidate (line)
  (let* ((words (split-string line " "))
         (text (car words))
         (meta (mapconcat 'identity (cdr words) " ")))
    (propertize text 'meta meta)))

(defun company-flow-parse-output (output)
  (mapcar 'company-flow-make-candidate
          (split-string output "\n")))

(defun company-flow-get-output (process)
  "Get the complete output of PROCESS."
  (with-demoted-errors "Error while retrieving process output: %S"
    (let ((pending-output (process-get process 'company-flow-pending-output)))
      (apply #'concat (nreverse pending-output)))))

(defun company-flow-receive-checker-output (process output)
  "Receive a syntax checking PROCESS OUTPUT."
  (push output (process-get process 'company-flow-pending-output)))

(defun company-flow-process-send-buffer (process)
  "Send all contents of current buffer to PROCESS.

Sends all contents of the current buffer to the standard input of
PROCESS, and terminates standard input with EOF."
  (save-restriction
    (widen)
    (process-send-region process (point-min) (point-max)))
  (process-send-eof process))

(defun company-flow-candidates-query (prefix callback)
  (let* ((default-directory "~/Source/the-link")
        (line (line-number-at-pos (point)))
        (col (+ 1 (column-number-at-pos (point))))
        (command (list "flow"
                       "autocomplete"
                       buffer-file-name
                       (number-to-string line)
                       (number-to-string col))))
    (setq process
          (apply 'start-process "company-flow" nil command))
    (setf (process-sentinel process) #'company-flow-handle-signal)
    (setf (process-filter process) #'company-flow-receive-checker-output)
    (process-put process 'company-flow-callback callback)
    (process-put process 'company-flow-prefix prefix)
    (company-flow-process-send-buffer process)))

(defun company-flow-prefix ()
  "Grab prefix for flow."
  (and (not (company-in-string-or-comment))
       (or (company-grab-symbol-cons "\\." 1)
           'stop)))

(defun company-flow-annotation (candidate)
  (format " %s" (get-text-property 0 'meta candidate)))

(defun company-flow-meta (candidate)
  nil)

(defun company-flow-doc (candidate)
  nil)

;;;###autoload
(defun company-flow (command &optional arg &rest _args)
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-flow))
    (prefix (company-flow-prefix))
    (annotation (company-flow-annotation arg))
    (meta (company-flow-meta arg))
    (doc-buffer (company-flow-doc arg))
    (ignore-case t)
    (sorted t)
    (candidates (cons :async
                      (lambda (callback)
                        (company-flow-candidates-query arg callback))))))

(provide 'company-flow)
