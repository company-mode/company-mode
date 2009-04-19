(require 'company)
(eval-when-compile (require 'cl))

(defun company-eclim-executable-find ()
  (let (file)
    (dolist (eclipse-root '("/Applications/eclipse" "/usr/lib/eclipse"
                            "/usr/local/lib/eclipse"))
      (and (file-exists-p (setq file (expand-file-name "plugins" eclipse-root)))
           (setq file (car (last (directory-files file t "^org.eclim_"))))
           (file-exists-p (setq file (expand-file-name "bin/eclim" file)))
           (return file)))))

(defcustom company-eclim-executable
  (or (executable-find "eclim") (company-eclim-executable-find))
  "*Location of eclim executable"
  :group 'company
  :type 'file)

(defcustom company-eclim-auto-save nil
  "*Determines whether to save the buffer when retrieving completions.
eclim can only complete correctly when the buffer has been saved."
  :group 'company
  :type '(choice (const :tag "Off" nil)
                 (const :tag "On" t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar company-eclim--project-dir 'unknown)
(make-variable-buffer-local 'company-eclim--project-dir)

(defvar company-eclim--project-name 'unknown)
(make-variable-buffer-local 'company-eclim--project-name)

(defvar company-eclim--doc nil)
(make-variable-buffer-local 'company-eclim--doc)

(defun company-eclim--buffer-lines ()
  (goto-char (point-max))
  (let (lines)
    (while (= 0 (forward-line -1))
      (push (buffer-substring-no-properties (point-at-bol) (point-at-eol))
            lines))
    lines))

(defun company-eclim--call-process (&rest args)
  (let ((coding-system-for-read 'utf-8))
    (with-temp-buffer
      (if (= 0 (apply 'call-process company-eclim-executable nil t nil
                      "-command" args))
          (company-eclim--buffer-lines)
        (message "Company-eclim command failed")
        nil))))

(defun company-eclim--project-list ()
  (mapcar (lambda (line) (nreverse (split-string line " *- *" nil)))
          (company-eclim--call-process "project_list")))

(defun company-eclim--locate-dominating-file (file name)
  (catch 'root
    (let ((dir (file-name-directory buffer-file-name)))
      (while (not (equal dir "/"))
        (when (file-exists-p (expand-file-name name dir))
          (throw 'root dir))
        (setq dir (file-name-directory (directory-file-name dir)))))))

(defun company-eclim--project-dir ()
  (if (eq company-eclim--project-dir 'unknown)
      (setq company-eclim--project-dir
            (directory-file-name
             (if (fboundp 'locate-dominating-file)
                 (expand-file-name (locate-dominating-file buffer-file-name
                                                           ".project"))
               (company-eclim--locate-dominating-file buffer-file-name
                                                     ".project"))))
    company-eclim--project-dir))

(defun company-eclim--project-name ()
  (if (eq company-eclim--project-name 'unknown)
      (setq company-eclim--project-name
            (car (cddr (assoc (company-eclim--project-dir)
                              (company-eclim--project-list)))))
    company-eclim--project-name))

(defun company-eclim--candidates (prefix)
  (interactive "d")
  (let ((project-file (file-relative-name buffer-file-name
                                          (company-eclim--project-dir)))
        (project-name (company-eclim--project-name)))
    (when company-eclim-auto-save
      (save-buffer)
      ;; FIXME: Sometimes this isn't finished when we complete.
      (company-eclim--call-process "java_src_update"
                                  "-p" (company-eclim--project-name)
                                  "-f" project-file))
    (setq company-eclim--doc
          (mapcar (lambda (line)
                    (cdr (split-string line "|" nil)))
                  (company-eclim--call-process
                   "java_complete" "-p" (company-eclim--project-name)
                   "-f" project-file
                   "-o" (number-to-string (1- (point)))
                   "-e" "utf-8"
                   "-l" "standard"))))
  (let ((completion-ignore-case nil))
    (all-completions prefix (mapcar 'car company-eclim--doc))))

(defun company-eclim (command &optional arg &rest ignored)
  "A `company-mode' completion back-end for eclim.
eclim provides access to Eclipse Java IDE features for other editors."
  (interactive (list 'interactive))
  (case command
    ('interactive (company-begin-backend 'company-eclim))
    ('prefix (and (derived-mode-p 'java-mode 'jde-mode)
                  buffer-file-name
                  company-eclim-executable
                  (company-eclim--project-name)
                  (not (company-in-string-or-comment))
                  (or (company-grab-symbol) 'stop)))
    ('candidates (company-eclim--candidates arg))
    ('meta (cadr (assoc arg company-eclim--doc)))
    ;; because "" doesn't return everything
    ('no-cache (equal arg ""))))

(provide 'company-eclim)
;;; company-eclim.el ends here
