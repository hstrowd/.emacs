;; ------------- CnuApp Utilities -----------------

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

(defun cnu-app-restart ()
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

(defun cnu-app-stop ()
  "Stops the cnuapp services."
  (interactive)
  ; This uses TRAMP to run the sudo command. I'm not really sure how it works. 
  ; I found it on a google group post.
  (with-temp-buffer
    (cd "/sudo::/")
    (let ((result (shell-command-to-string "sudo sv d /var/service/*")))
      (if (= 0 (length result))
	  (message "Success: The app is now being stopped")
	(error "Failed: Unable to restart the app. Error: %s" result)))))

(defun cnu-app-start ()
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


(provide 'cnuapp-utils)