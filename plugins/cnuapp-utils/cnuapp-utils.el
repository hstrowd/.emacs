;; ------------- CnuApp Utilities -----------------

(setq cnuapp-log-dir "/var/log/cnuapp/")
(setq log-sub-dirs '(services.d space.d lsws.d))
(setq cnuapp-dir "/export/web/cnuapp/")

(defun cnu-get-env ()
  "Returns the current symlink for the /etc/cnu/cnu_env file."
  (interactive)
  (let ((result (shell-command-to-string "ls -la /etc/cnu/cnu_env")))
    (if (string-match "/etc/cnu/cnu_env\\.[A-Z]+" result)
	(let ((cluster-index (string-match "/etc/cnu/cnu_env\\.[A-Z]+" result)))
	  (chomp (substring result (+ cluster-index 17))))
      (error "FAILED: Unable to identify the current environment.\nLookup result: %s" result)
      nil)))

(defun cnu-set-env (country)
  "Changes the symlink for the /etc/cnu/cnu_env file to point
to the requested country."
  (interactive "MCountry: ")
  (let ((country-file (format "/etc/cnu/cnu_env.%s" (upcase country))))
    (if (file-exists-p country-file)
	(progn
	  (call-process "ln" nil "*Messages*" nil "-fs" country-file "/etc/cnu/cnu_env")
	  (message "Success: Changed environment to %s" country-file)
	  t)
      (error "Failed: File '%s' does not exist." country-file)
      nil)))

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


;; Database Utilities
(require 'sql)

(add-to-list 'load-path "~/.emacs.d/plugins/sql-utils")
(require 'sql-utils)

(defun cnu-connect-to-prod-db (db_suffix)
  "Establishes a connection to the specified production database."
  (interactive "MDB suffix: ")
  (setq db_suffix (if (string= db_suffix "gb")
		      "uk"
		    (downcase db_suffix)))

  ; Set the DB connection settings.
  (setq sql-user "hstrowd"
	sql-database (if (string= db_suffix "us")
			 "cnuapp_prod"
		       (concat "cnuapp_prod_" db_suffix)))

  ; Identify the server based on the requested database.
  (if (string= db_suffix "us")
      (setq sql-server "slavedb3.cashnetusa.com"
	    sql-database "cnuapp_prod")
    (if (string= db_suffix "uk")
	(setq sql-server "slavedb.quickquid.co.uk")
      (if (string= db_suffix "au")
	  (setq sql-server "slavedb.dollarsdirect.com.au")
	(if (string= db_suffix "ca")
	    (setq sql-server "slavedb.dollarsdirect.ca")
	  (if (string= db_suffix "jv")
	      (setq sql-server "slavedbjv.cashnetusa.com")
	    (error "FAILURE: Unable to identify the server based on the provided db_siffix"))))))

  ; Connect to the DB.
  (connect-to-psql-db sql-database))


(defun cnu-connect-to-dev-db (db_suffix)
  "Establishes a connection to the specified development database."
  (interactive "MDB suffix: ")
  (setq db_suffix (if (string= db_suffix "uk")
		      "gb"
		    (downcase db_suffix)))

  ; Set the DB connection settings.
  (setq sql-user "cnuapp"
	sql-database (if (string= db_suffix "us")
			 "cnuapp_dev"
		       (concat "cnuapp_dev_" db_suffix))
	sql-server "localhost")
  ; Connect to the DB.
  (connect-to-psql-db sql-database))

;; TODO: create a function for selecting the current sql-buffer.


;; Console Utilities
(defun cnu-console (cluster)
  "Starts a cnuapp console for the specified cluster."
  (interactive "MCluster: ")

  ; Identify the requested environment.
  (setq cur-env (cnu-get-env))
  (if (not (cnu-set-env (upcase cluster)))
      (error "FAILED: Unable to set the environment to cluster %s." cluster)
    ; Identify the name of the buffer to be used for this console.
    (setq buf-name (concat cluster "_console"))
    (if (get-buffer buf-name)
	(progn
	  (setq index 1)
	  (while (get-buffer (concat buf-name (number-to-string index)))
	    (setq index (1+ index)))
	  (setq buf-name (concat buf-name (number-to-string index)))))

    ; Start the console.
    (shell (get-buffer-create buf-name))
    (pop-to-buffer buf-name)
    (process-send-string nil "/export/web/cnuapp/script/console\n")
    (sleep-for 4)
    (cnu-set-env cur-env)))

(provide 'cnuapp-utils)


