;; RGM is a password storage mode for Emacs. It uses an encrypted
;; file to store the passwords

(require 'json)
(require 'assoc)

(defconst rgm-default-file "~/Dropbox/rgm.dat")
(defconst rgm-config-file "~/.rgmrc")

(defvar rgm-mode-map nil "Local keymap for rgm mode")

(defun init-rgm-mode-map ()
    (setq rgm-mode-map (make-sparse-keymap))
    (define-key rgm-mode-map "a" 'rgm-credentials-add-cmd)
    (define-key rgm-mode-map "d" 'rgm-credentials-delete-cmd)
    (define-key rgm-mode-map "p" 'rgm-credentials-get-password-cmd)
    (define-key rgm-mode-map "\M-p" 'rgm-credentials-show-password-cmd)
    (define-key rgm-mode-map "l" 'rgm-credentials-get-username-cmd)
    (define-key rgm-mode-map "c" 'rgm-change-master-password-cmd)
    (define-key rgm-mode-map "e" 'rgm-credentials-update-cmd)
    (define-key rgm-mode-map "h" 'rgm-show-help-buffer-cmd))

(defun rgm-show-help-buffer-cmd ()
  (interactive)
  (let* ((current-window (selected-window))
        (help-window (split-window current-window)))
    (pop-to-buffer "*RGM help*"  help-window)
    (insert "h - Display this help\n")
    (insert "a - Add credentials for new site\n")
    (insert "d - Delete credentials for selected site\n")    
    (insert "p - Copy site's password to clipboard\n")
    (insert "M-p - Message (minibuffer and *Messages*) site's password\n")
    (insert "l - Copy site's username (login) to clipboard\n")
    (insert "c - Change master password\n")    
    (insert "e - Edit credentials for selected site\n")
    (setq buffer-read-only t)
    (select-window current-window)))

(define-derived-mode rgm tabulated-list-mode "rgm" "Password storage mode"
  (setq tabulated-list-format [("Site" 20 t)
                               ("Username" 25 t)
                               ("Description"  30 t)])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key (cons "Site" nil))
  (tabulated-list-init-header))

(defun rgm-mode ()
  "Password storage mode"
  (interactive)
  (pop-to-buffer "*RGM*" nil)
  (kill-all-local-variables)
  (rgm)
  (delete-other-windows)
  (setq buffer-read-only t)
  (init-rgm-mode-map)
  (use-local-map rgm-mode-map)
  (add-hook 'kill-buffer-hook 'rgm-cleanup t t)
  (rgm-init-data)
  (rgm-read-and-render-credentials)
  (rgm-init-refresh-timer)
  (message "Press h for help"))

(defun rgm-cleanup ()
  (message "rgm-mode shutting down")
  (setq rgm-mode-map nil)
  (if (boundp 'refresh-timer)
      (cancel-timer refresh-timer))
  (kill-all-local-variables))

(defun rgm-init-data ()
  (make-local-variable 'rgm-file)
  (make-local-variable 'credentials)
  (make-local-variable 'password)
  (make-local-variable 'refresh-timer)
  (rgm-read-password)
  (setq rgm-file (get-rgm-file)))


(defun rgm-read-and-render-credentials ()
  (setq credentials (rgm-read-credentials-interactive rgm-file))
  (setq tabulated-list-entries  (rgm-credentials-tabulated-list-entries credentials))
  (tabulated-list-print t))

(defun rgm-refresh-timed-task ()
    (undo-boundary)
    (message "Refreshing credential data from file")
    (rgm-read-and-render-credentials)
    (undo-boundary))

(defun rgm-init-refresh-timer ()
  (setq refresh-timer (run-at-time "30 min" 1800 'rgm-refresh-timed-task)))

(defun rgm-credentials-tabulated-list-entries (credentials)
  (let (result-list)
    (dolist (cred-elem credentials result-list)
      (setq result-list 
	    (cons (list 
		   (rgm-elem-site cred-elem) 
		   (vector 
		    (rgm-elem-site cred-elem)
		    (rgm-elem-user cred-elem)
		    (rgm-elem-description cred-elem))) 
		  result-list)))))

(defun rgm-elem-site (cred-elem)
  (car cred-elem))

(defun rgm-elem-description (cred-elem)
  (elt (cdr cred-elem) 0))

(defun rgm-elem-user (cred-elem)
  (elt (cdr cred-elem) 1))

(defun rgm-elem-password (cred-elem)
  (elt (cdr cred-elem) 2))

(defun rgm-read-password (&optional prompt)
  (unless prompt (setq prompt "Password: "))
  (setq password (read-passwd prompt)))

(defun rgm-render-credentials ()
  (setq tabulated-list-entries  (rgm-credentials-tabulated-list-entries credentials))
  (tabulated-list-print t))

(defun rgm-credentials-add-cmd (site description username site-password)
  (interactive "sSite: \nsDescription: \nsUsername: \nsPassword: ")
  (rgm-credentials-add site description username site-password)
  (rgm-save-and-refresh))

(defun rgm-credentials-delete-cmd ()
  (interactive)
  (let ((to-delete (tabulated-list-get-id))
        (confirmation  (read-from-minibuffer "Really delete? [y/n]: ")))
    (when (and to-delete (equal "y" confirmation))
      (rgm-credentials-delete to-delete)
      (rgm-save-and-refresh))))

(defun rgm-get-current-line-elem ()
  (let* ((creds-name (tabulated-list-get-id))
         (rgm-elem (assoc creds-name credentials)))
    rgm-elem))

(defun rgm-credentials-delete (site)
  (setq credentials (delq (assoc site credentials) credentials)))

(defun rgm-credentials-update-cmd ()
  (interactive)
  (let* ((site (tabulated-list-get-id))
         (cred-elem (assoc site credentials))
         (new-desc)
         (new-user)
         (new-pass))
    (when cred-elem
      (setq new-desc (rgm-read-cred-fragment "Description" cred-elem 'rgm-elem-description))
      (setq new-user (rgm-read-cred-fragment "Username" cred-elem 'rgm-elem-user))
      (setq new-pass (rgm-read-cred-fragment "Password" cred-elem 'rgm-elem-password))
      (rgm-credentials-delete site)
      (rgm-credentials-add site new-desc new-user new-pass)
      (rgm-save-and-refresh))))

(defun rgm-read-cred-fragment (prompt cred-elem accessor)
  "Defaults to old value"
  (let ((new-value)
        (old-value (funcall accessor cred-elem)))
    (setq new-value (read-from-minibuffer (concat prompt " [" old-value "]:")))
    (if (not (equal "" (trim-string new-value)))
        new-value
      old-value)))

(defun rgm-credentials-get-password-cmd ()
  (interactive)
  (let ((rgm-elem (rgm-get-current-line-elem)))
    (when rgm-elem
      (kill-new (rgm-elem-password rgm-elem))
      (message "Password copied to clipboard"))))

(defun rgm-credentials-show-password-cmd ()
  (interactive)
  (let ((rgm-elem (rgm-get-current-line-elem)))
    (when rgm-elem
      (message (rgm-elem-password rgm-elem)))))

(defun rgm-credentials-get-username-cmd ()
  (interactive)
  (let ((elem (rgm-get-current-line-elem)))
    (when elem
      (kill-new (rgm-elem-user elem))
      (message "Username copied to clipboard"))))

(defun rgm-change-master-password-cmd ()
  (interactive)
  (rgm-read-password "New password: ")
  (rgm-save-and-refresh))

(defun rgm-save-and-refresh ()
  (setq credentials (sort credentials (lambda (a b) (string< (car a) (car b)))))
  (rgm-render-credentials)
  (rgm-write-credentials rgm-file credentials password))

(defun rgm-credentials-add (site description username password)
  (aput 'credentials site (vector description username password)))

(defun store-new-rgm-file-path(err-msg)
  (when (not (file-writable-p rgm-config-file))
    (error "Can't write to %s" rgm-config-file))
  (setq rgm-file (read-from-minibuffer (concat err-msg "Path to file where you want to store passwords: ")))
  (when (file-writable-p rgm-file)
      (rgm-write-string-to-file rgm-file rgm-config-file)
    t))

(defun get-rgm-file ()
  (if (file-exists-p rgm-config-file)
      (rgm-read-filename-from-config-file)
    (let ((success nil) (err-msg ""))
      (while (not success)
        (setq success (store-new-rgm-file-path err-msg))
        (setq err-msg (concat "File " rgm-file " is not valid! ")))
       (rgm-read-filename-from-config-file))))

(defun rgm-read-filename-from-config-file ()
  (trim-string (rgm-read-string-from-file rgm-config-file)))

(defun rgm-read-credentials-interactive (rgm-file &optional pwd-input-func)
  "Reads credentials and prompts for password if it's wrong retrying in a loop"
  (unless pwd-input-func
    (setq pwd-input-func (lambda () (rgm-read-password "Wrong password, try again: "))))
  (let ((success) (credentials))
    (while (not success)
      (condition-case e
          (progn 
            (setq success t)
            (setq credentials (rgm-read-credentials rgm-file password)))
        (error
         (if (string-match "bad decrypt" (error-message-string e))
             (progn 
               (setq success nil)
               (funcall pwd-input-func))
               (error (error-message-string e))))))
    credentials))

(defun rgm-read-credentials (rgm-file password)
  (unless (file-exists-p rgm-file)
    (rgm-write-credentials rgm-file '() password))
  (let ((json-key-type 'string) (creds) (cred-string))
    (setq cred-string (rgm-decrypt rgm-file password))
    (setq creds (json-read-from-string cred-string))
    (sort creds (lambda (a b) (string< (car a) (car b))))))

(defun rgm-write-credentials (rgm-file credentials password)
  (rgm-encrypt (json-encode credentials) rgm-file password))
   
(defun rgm-encrypt (contents outputfile password)
  (let ((tempfile (rgm-write-to-tempfile contents)))
    (rgm-encrypt-file tempfile outputfile password)
    (delete-file tempfile)))

(defun rgm-decrypt (inputfile password)
  (let ((tempfilename (rgm-create-tempfilename))
        (plaintext))
    (rgm-decrypt-file inputfile tempfilename password)
    (setq plaintext (rgm-read-string-from-file tempfilename))
    (delete-file tempfilename)
    plaintext))

(defun rgm-encrypt-file (inputfile outputfile passwd)
  (rgm-call 
   (lambda () (call-process "openssl" nil t nil "enc" "-aes-256-cbc" "-a" "-salt" "-in" inputfile "-out" outputfile "-pass" (concat "pass:" passwd))) "Could not encrypt file: "))

(defun rgm-decrypt-file (inputfile outputfile passwd)
    (rgm-call 
     (lambda () (call-process "openssl" nil t nil "enc" "-d" "-aes-256-cbc" "-a" "-in" inputfile "-out" outputfile "-pass" (concat "pass:" passwd))) "Could not decrypt file: "))

(defun rgm-get-creds-name (line)
  (let ((success (string-match "\\(.*\\)[[:space:]]+->" line)))
    (if success
        (match-string 1 line)
      nil)))

(defun rgm-read-string-from-file (file)
  "Return file content as string"
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(defun rgm-write-string-to-file (string file)
   (with-temp-buffer
     (insert string)
     (when (file-writable-p file)
       (write-region (point-min) (point-max) file nil 1))))

(defun rgm-write-to-tempfile (contents)
  "Returns the filename where string contents were written"
  (let ((filename (rgm-create-tempfilename)))
    (rgm-write-string-to-file contents filename)
    filename))

(defun rgm-call (func err-prefix)
  (with-temp-buffer 
    (unless (= 0 (funcall func))
        (error (concat err-prefix (buffer-string))))))

(defun rgm-create-tempfilename ()
  (concat temporary-file-directory (make-temp-name  "rgm")))

(defmacro rgm-with-writeonly-off (&rest body)
  `(progn
     (save-excursion
       (setq buffer-read-only nil)
       ,@body
       (setq buffer-read-only t))))

(defun trim-string (string)
  "Remove white spaces in beginning and ending of STRING.
White space here is any of: space, tab, emacs newline (line feed, ASCII 10)."
  (replace-regexp-in-string "\\`[ \t\n]*" "" (replace-regexp-in-string "[ \t\n]*\\'" "" string)))

(provide 'rgm) ;; For require
