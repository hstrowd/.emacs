;; ------------- CnuApp Utilities -----------------

(setq cnuapp-log-dir "/var/log/cnuapp/")
(setq log-sub-dirs '(services.d space.d lsws.d))
(setq cnuapp-dir "/export/web/cnuapp/")

(defun cnu-change-env (country)
  "Changes the symlink for the /etc/cnu/cnu_env file to point
to the requested country"
  (interactive "MCountry: ")
  (let ((country-file (format "/etc/cnu/cnu_env.%s" (upcase country))))
    (if (file-exists-p country-file)
	(progn
	  (call-process "ln" nil "*Message*" nil "-fs" country-file "/etc/cnu/cnu_env")
	  (message "Success: Changed environment to %s" country-file))
      (error "Failed: File '%s' does not exist." country-file))))

;; TODO: Finish this.
(defun run-sudo-sv (cmd dir)
  ""
  ; This uses TRAMP to run the sudo command. I'm not really sure how it works. 
  ; I found it on a google group post.
  (cd "/sudo::/")
  (let ((result (shell-command-to-string (format "sudo sv %s" cmd))))
    (if (= 0 (length result))
	(message "Success: The app is now being restarted")
      (error "Failed: Unable to restart the app. Error: %s" result))))

(defun cnu-restart-app ()
  "Restarts the cnuapp services."
  (interactive)
  ; This uses TRAMP to run the sudo command. I'm not really sure how it works. 
  ; I found it on a google group post.
  (with-temp-buffer
    (cd "/sudo::/")
    (let ((result (shell-command-to-string "sudo sv d /var/service/* && sudo sv u /var/service/*")))
      (if (= 0 (length result))
	  (message "Success: The app is now being restarted")
	(error "Failed: Unable to restart the app. Error: %s" result)))))

(defun cnu-stop-app ()
  "Stops the cnuapp services."
  (interactive)
  ; This uses TRAMP to run the sudo command. I'm not really sure how it works. 
  ; I found it on a google group post and it appears to work.
  (with-temp-buffer
    (cd "/sudo::/")
    (let ((result (shell-command-to-string "sudo sv d /var/service/*")))
      (if (= 0 (length result))
	  (message "Success: The app is now being stopped")
	(error "Failed: Unable to restart the app. Error: %s" result)))))

(defun cnu-start-app ()
  "Starts the cnuapp services."
  (interactive)
  ; This uses TRAMP to run the sudo command. I'm not really sure how it works. 
  ; I found it on a google group post.
  (with-temp-buffer
    (cd "/sudo::/")
    (let ((result (shell-command-to-string "sudo sv u /var/service/*")))
      (if (= 0 (length result))
	  (message "Success: The app is now being started")
	(error "Failed: Unable to restart the app. Error: %s" result)))))


(defun cnu-auto-clean-house ()
  "Invokes the function to clean all cnuapp garbage."
  (interactive)
  (cnu-clean-house))

(defun cnu-clean-house (&optional skip-p4-tmp skip-logs skip-test-results)
  "Deletes temporary files, cnuapp logs, and unit test results."
  (interactive "MSkip p4 temporary files(y/n)? \nMSkip cnuapp logs(y/n)? \nMSkip test results (y/n)?")
  (if (or (not skip-p4-tmp) 
	  (equal (downcase skip-p4-tmp) "false")
	  (equal (downcase skip-p4-tmp) "f")
	  (equal (downcase skip-p4-tmp) "no")
	  (equal (downcase skip-p4-tmp) "n"))
      (kill-p4-temp-files))
  (if (or (not skip-logs)
	  (equal (downcase skip-p4-tmp) "false")
	  (equal (downcase skip-p4-tmp) "f")
	  (equal (downcase skip-p4-tmp) "no")
	  (equal (downcase skip-p4-tmp) "n"))
      (kill-cnuapp-logs))
  (if (or (not skip-test-results)
	  (equal (downcase skip-p4-tmp) "false")
	  (equal (downcase skip-p4-tmp) "f")
	  (equal (downcase skip-p4-tmp) "no")
	  (equal (downcase skip-p4-tmp) "n"))
      (kill-cnuapp-unit-test-results))
  (message "Sucess: All temporary files have been deleted."))

(defun kill-p4-temp-files ()
  "Deletes the following p4 temporary files, if found:
  - branch backups
  - changelist specs"
  (interactive)

  (if p4-chglst-file-prefix
      (let ((rm-result (shell-command-to-string (format "rm %s*" p4-chglst-file-prefix))))
	(if (rm-failed? rm-result) 
	    (error "Failed: Unable to remove p4 temporary files from %s. Error: %s"
		   p4-chglst-file-prefix rm-result))))

  (if p4-branch-backup-dir
      (let ((rm-result (shell-command-to-string (format "rm -r %s*" p4-branch-backup-dir))))
	(if (rm-failed? rm-result) 
	    (error "Failed: Unable to remove p4 branch backups from %s. Error: %s" 
		   p4-branch-backup-dir rm-result)))))

(defun kill-cnuapp-logs ()
  "Deletes all cnuapp logs. Including all logs from the following sub-directories:
  - services.d
  - space.d
  - lsws.d"
  (interactive)
  (let ((rm-result (shell-command-to-string (format "rm -r %s*" cnuapp-log-dir))))
    (if (rm-failed? rm-result) 
	(error "Failed: Unable to clear cnuapp log directory: %s. Error: %s" 
	       cnuapp-log-dir rm-result)))

; TODO: Should I be removing these files?
;  (dolist (sub-dir log-sub-dirs)
;    (let ((rm-result (shell-command-to-string (format "rm %s%s/@4*" cnuapp-log-dir sub-dir))))
;      (if (rm-failed? rm-result) 
;	  (error "Failed: Unable to clear cnuapp log directory: %s%s. Error: %s" 
;		 cnuapp-log-dir sub-dir rm-result))))
  )

(defun kill-cnuapp-unit-test-results ()
  "Deletes all cnuapp unit test results."
  (let ((rm-result (shell-command-to-string (format "rm -r %sresult*" cnuapp-dir))))
    (if (rm-failed? rm-result) 
	(error "Failed: Unable to clear cnuapp unit test results from %s. Error: %s" 
	       cnuapp-dir rm-result))))

(defun rm-failed? (rm-result)
  "Checks if the provide string indicates a rm failure."
  (dolist (result-line (split-string rm-result "\n"))
    (if (and (> (length result-line) 0) 
	     (not (string-match ": No such file or directory" result-line))
	     (not (string-match ": Is a directory" result-line)))
	(return t))))


(provide 'cnuapp-utils)