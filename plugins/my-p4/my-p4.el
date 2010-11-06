;; TODO: Add proper heading

;; ------------ Plugin Variables --------------

(setq p4-tmp-buf-name "*p4-tmp*")
(setq p4-state-file "/home/cnuapp/.emacs.d/plugins/my-p4/p4.state")
(setq p4-chglst-tmplt "/home/cnuapp/.emacs.d/plugins/my-p4/p4-init-changelist")
(setq p4-chglst-file-prefix "/home/cnuapp/.emacs.d/plugins/my-p4/changelist-specs/chnglst.")
(setq p4-chglst-file-postfix ".log")
(setq p4-branch-backup-dir "/home/cnuapp/.emacs.d/plugins/my-p4/branch-backups/")


;; ------------ Helper Functions --------------

(defun convert-web-to-comp-stable (file-path)
  "Converts a file in /export/web/... to /export/comp/stable/...
If the provided file is not under the /export/web/ directory it is
returned unchanged."
  (if (equal (substring file-path 0 12) "/export/web/")
      (concat "/export/comp/stable/" (substring file-path 12))
    file-path))

(defun clear-buffer (buffer-or-name)
  "Deletes all content from the provided buffer.

An error is thrown in any of the following circumstances:
  - No buffer is associated with the provided buffer-name."
  (interactive "bBuffer: ")
  (let ((buffer (get-buffer buffer-or-name)))
    (if buffer
	(save-excursion
	  (set-buffer buffer)
	  (erase-buffer))
      (error "Failed: No %s buffer exist." buffer-or-name))))

(defun replace-all (text replacement)
  "Replaces all instances of the TEXT with the REPLACEMENT in the 
current buffer."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward text nil t)
      (replace-match replacement nil t))))

(defun p4-get-depot-path (local-file)
  "Identifies the full depot path of the latest revision synced of the provided file.

An error is thrown in any of the following circumstances:
  - The latest revision of the provided file cannot be identified."
  ; Run a have command.
  (let ((have-result (shell-command-to-string (format "p4 have %s" local-file))))
    ; Parse the full depot path out of the result.
    (if (string-match "#[0-9]+ - /export/comp/stable/" have-result)
	(substring have-result 0 (match-beginning 0))
      (error "Failed: Unable to identify the latest revision synced of: %s.\nError: %s" 
	     local-file have-result))))

(defun p4-get-bug-branch-path (rel-path-to-file bug-branch)
  "Identifies the path to the specified file on the provided bug branch."
  (concat "//depot/cnuapp/bug/" bug-branch "/" rel-path-to-file))

(defun p4-new-change (changelist-content-file)
  "Creates a new p4 changelist and returns the number.
An error is thrown in any of the following circumstances:
  - The changelist is not able to be created."
  ; Construct a new p4 changelist.
  (with-temp-buffer
    ; This must use call-process, because it needs to have input provided in a file.
    (call-process "p4" changelist-content-file (current-buffer) nil "change" "-i")
    ; Identify the id number of the newly created changelist.
    (goto-char (point-max))
    (if (re-search-backward "Change [0-9]+ created." nil t)
	(progn
	  (goto-char (+ (point) 7))
	  (set-mark (point))
	  (re-search-forward "[0-9]+")
	  (buffer-substring (mark) (point)))
      (error "P4 new change: Unable to create a changelist. Error: %s" (buffer-substring (point-min) (point-max))))))


(setq active-changelist-key "active_changelist")
(setq files-to-branch-key "files_to_branch")
(setq bug-branch-key "bug_branch")
(setq p4-list-separator ",")

(defun p4-state-get (key)
  "Returns the value stored in the p4 state file that corresponds
to the provided key. If the key is not found or the value is empty, 
nil is returned."
  (let 	((p4-state-buffer (find-file p4-state-file)))
    (save-excursion 
      ; Identify the value corresponding to the provided key.
      ;; TODO: update this to use setq
      (set-buffer p4-state-buffer)
      (save-excursion
	; Find the position of the key in the state file.
	(goto-char (point-min))
	(if (re-search-forward (format " -- %s: \\[" key) nil t)
	    (progn
	      ; Isolate the value corresponding to the key.
	      (set-mark (point))
	      (end-of-line)
	      (if (re-search-backward "\\]" (mark) t)
		  (let ((value (buffer-substring (mark) (point))))
		    ; Close the state file and return the value.
		    (kill-buffer p4-state-buffer)
		    (if (= 0 (length value))
			(progn
			  (message "P4 state lookup: %s value is empty." key)
			  nil)
		      value))
		(message "P4 state lookup: No end ']' for %s value." key)
		(kill-buffer p4-state-buffer)
		nil))
	  (message "P4 state lookup: %s key not found." key)
	  (kill-buffer p4-state-buffer)
	  nil)))))

(defun p4-state-set (key value)
  "Updates the value stored in the p4 state file that corresponds
to the provided key, setting it to the value specified.
If the key is not found, it is added to the file."
  (let 	((p4-state-buffer (find-file p4-state-file)))
    (save-excursion 
      ; Identify the location of the provided key in the p4 state 
      ; file.
      ;; TODO: update this to use setq
      (set-buffer p4-state-buffer)
      (save-excursion
	(goto-char (point-min))
	(if (re-search-forward (format " -- %s: \\[" key) nil t)
	    (progn
	      ; Delete the current value.
	      (set-mark (point))
	      (end-of-line)
	      (delete-region (mark) (point))
	      ; Insert the new value.
	      (insert value "]"))

	  ; If the key is not found, add it to the end of the
	  ; file.
	  (goto-char (point-max))
	  (insert (format " -- %s: [%s]\n" key value)))
	; Save the state file.
	(write-file p4-state-file)
	(kill-buffer p4-state-buffer)))))

(defun p4-state-push (key value)
  "Appends the provided value to the end of the list of values that
correspond to the provided key."
  (p4-state-set key (concat (p4-state-get key) value p4-list-separator)))



;; ---------- Interactive Functions -----------

(defun p4-edit ()
  "Check out the file corresponding to the current buffer.

An error is thrown in any of the following circumstances:
  - The p4 edit command on the file corresponding to this buffer fails."
  (interactive)
  (let ((local-file (convert-web-to-comp-stable (buffer-file-name))))
    ; Attempt to check-out the file
    (let ((edit-result (shell-command-to-string (format "p4 edit %s" local-file))))
      (if (string-match "#[0-9]+ - opened for edit" 
			edit-result)
          ; If successful, make the buffer read-only.
	  (progn 
	    (setq buffer-read-only nil)
	    (message "Success: Checked out %s" local-file))
	; If unsuccessful, throw an error.
	(error "Failed: Unable to check out %s.\nError: %s" 
	       local-file edit-result)))))

(defun p4-revert ()
  "Reverts the file corresponding to the current buffer.

An error is thrown in any of the following circumstances:
  - The p4 revert command on the file corresponding to this buffer fails."
  (interactive)
  (let ((local-file (convert-web-to-comp-stable (buffer-file-name))))
    ; Attempt to check-out the file
    (let ((edit-result (shell-command-to-string (format "p4 revert %s" local-file))))
      (if (string-match "#[0-9]+ - was \\(edit\\|integrate\\|delete\\), reverted"
			edit-result)
          ; If successful, make the buffer read-only.
	  (progn 
	    (setq buffer-read-only t)
	    (revert-buffer t t)
	    (message "Success: Reverted %s" local-file))
	; If unsuccessful, throw an error.
	(error "Failed: Unable to revert %s.\nError: %s" 
	       local-file edit-result)))))

(defun p4-rebase ()
  "Rebases the current file by integrating any changes from comp/stable.

An error is thrown in any of the following circumstances:
  - The last revision of the current file cannot be identified.
  - The p4 integration from comp/stable to the current file fails."
  (interactive)
  (let ((local-file (convert-web-to-comp-stable (buffer-file-name))))
    ; Identify the currently synced revision and the comp/stable revision
    ; Attempt to integrate the corresponding file from comp/stable
    (let ((cur-sync-depot-file (p4-get-depot-path local-file))
	  (comp-stable-depot-file (format "//depot/cnuapp/%s" (substring local-file 8))))
      (let ((integrate-result (shell-command-to-string (format "p4 integrate -3 %s %s" 
							       comp-stable-depot-file 
							       cur-sync-depot-file))))
	(if (string-match (format "%s#[0-9]+ - integrate from %s#[0-9]+" 
				  cur-sync-depot-file 
				  comp-stable-depot-file)
			  integrate-result)
	    (message "Success: %s has been integrated into %s. Please resolve any conflicts." 
		     comp-stable-depot-file 
		     cur-sync-depot-file)
	  ; Check if all revisions have been integrated, because that
	  ; isn't a failure.
	  (if (string-match " - all revision(s) already integrated." 
			    integrate-result)
	      (message "All revision(s) already integrated.")
	    (error "Failed: Unable to integrate from %s to %s. Error: %s" 
		   comp-stable-depot-file 
		   cur-sync-depot-file
		   integrate-result)))))))

(defun p4-create-changelist (issue-number)
  "Creates a new changelist to be used by the my-p4 plugin.

An error is thrown in any of the following circumstances:
  - If an active changelist already exist.
  - If a new changelist is unable to be created."
  (interactive "MIssue number: ")
  (let ((p4-chglst-buffer (find-file-noselect p4-chglst-tmplt))
	(p4-chglst-file (concat p4-chglst-file-prefix 
				issue-number "-" 
				(format-time-string "%Y%m%d%H%M%S") 
				p4-chglst-file-postfix))
	(active-changelist (p4-state-get active-changelist-key))
	(bug-branch (concat issue-number "_sparse")))
    (if active-changelist
	(error "The my-p4 changelist is already set to: %s" active-changelist))

    (save-excursion
      ; Create the changelist file for this change.
      (set-buffer p4-chglst-buffer)
      (replace-all "<client-name>" p4-client-name)
      (replace-all "<user-name>" p4-user-name)
      (replace-all "<bug-branch>" bug-branch)
      (replace-all "<source-branch>" "comp/stable")
      (replace-all "<issue-number>" issue-number)
      (write-region (point-min) (point-max) p4-chglst-file nil nil nil t)

      ; Revert the changelist template and close the buffer.
      (revert-buffer t t)
      (kill-buffer p4-chglst-buffer)

      ; Construct the changelist and store it in the p4 state file.
      (let ((changelist-num (p4-new-change p4-chglst-file)))
	(p4-state-set active-changelist-key changelist-num)
	(p4-state-set bug-branch-key bug-branch)
	(message "Success: Created and saved changelist: %s" changelist-num)))))

(defun p4-mark-to-branch ()
  "Marks the file corresponding to the current buffer to be branched
as part of the active changelist.
An error is thrown in any of the following circumstances:
  - The current buffer does not correspond to a file in the p4 client."
  (interactive)
  (let ((local-file (convert-web-to-comp-stable (buffer-file-name))))
    ; Verify that this file is in fact within the source tree.
    (if (equal (substring local-file 0 20) "/export/comp/stable/")
	(let ((rel-file-path (substring local-file 20)))
	  ; TODO: Check if the file is already in the list.
          ; Append this file to the end of the list of files to be branch.
	  (p4-state-push files-to-branch-key rel-file-path)
	  (message "Success: File %s was added to the list of files to be branched."
		   local-file))
      (error "Failed: File not in p4 client.\nProvided file: %s" local-file))))

(defun p4-clear-files-to-branch ()
  "Clears the list of files to be branched in the active changelist.
This list of files is stored in the my-p4 state file."
  (interactive)
  (p4-state-set files-to-branch-key ""))

;; TODO: test this under all conditions.
(defun p4-branch-filse ()
  "Branches all files currently stored in the my-p4 plugin state file
under the 'files_to_branch' field to the branch specified by the active
changelist of the my-p4 plugin.

Note: This is not intended to branch deletions.

This will result in an error if:
  - There is no active changelist.
  - If there is an error in copying the files to the backup directory."
  (interactive)
  ; Identify the files currently marked to be branched.
  (let ((files-to-branch (p4-state-get files-to-branch-key))
	(branch (p4-state-get bug-branch-key))
	(active-changelist (p4-state-get active-changelist-key))
    (dolist (file files-to-branch)
      (let ((local-file (format "/export/comp/stable/%s" file))
	    (backup-file ((concat p4-branch-backup-dir
				  "/"
			          branch "-" (format-time-string "%Y%m%d%H%M%S")
				  "/"
				  file)))
	    (dest-depot-path (p4-get-bug-branch-path file branch)))

        ; Copy each file to be branched to a temporary directory.
	(let ((copy-result (shell-command-to-string "cp %s %s" local-file backup-file)))
	  (if (/= 0 (length copy-result))
	      (error "Failed: Unable to copy file %s to the backup directory (%s).\nError: %s"
		     file p4-branch-backup-dir copy-result)))

        ; Revert each file to be branched.
	(let ((revert-result (shell-command-to-string "p4 revert %s" local-file)))
	  (if (not (string-match (format "%s#[0-9]+ - was edit, reverted" file)
				 revert-result))
	      (error "Failed: Unable to revert file %s.\nError: %s"
		     file revert-result)))

        ; Integrate each file to be branched to the bug branch.
	(let ((integrate-result 
	       (shell-command-to-string "p4 integrate -3 -c %s %s %s" 
					active-changelit local-file dest-depot-path)))
	  ; TODO: figure out what this regex should be.
;	  (if (not (string-match (format "%s#[0-9]+ - ??? %s" file dest-depot-path)
;				 integrate-result))
;	      (error "Failed: Unable to integrate file %s to %s.\nError: %s"
;		     file dest-depot-path integrate-result))
      )))

    ; Submit the changelist containing the branching of each file.
    (let ((submit-result (shell-command-to-string "p4 submit -c %s" active-changelist)))
      ; TODO: figure out what this regex should be.
;      (if (not (string-match "???" submit-result))
;	  (error "Failed: Unable to sumbit changelist %s.\nError: %s"
;		 active-changelist submit-result))
      (p4-state-set active-changelist-key ""))
 

    (dolist (file files-to-branch)
      (let ((local-file (format "/export/comp/stable/%s" file))
	    (backup-file ((concat p4-branch-backup-dir
				  "/"
			          branch "-" (format-time-string "%Y%m%d%H%M%S")
				  "/"
				  file)))
	    (dest-depot-path (p4-get-bug-branch-path file branch)))

        ; Check out the files on the bug branch.
	(let ((edit-result (shell-command-to-string "p4 edit %s" dest-depot-path)))
	  (if (not (string-match (format "%s#[0-9]+ - opened for edit" edit-result)))
	      (error "Failed: Unable to open file %s for edit.\nError: %s"
		     dest-depot-pathedit-result)))

        ; Copy the files back into the source tree.
	(let ((copy-result (shell-command-to-string "cp %s %s" backup-file local-file)))
	  (if (/= 0 (length copy-result))
	      (error "Failed: Unable to copy file %s from the backup directory (%s) into the source tree.\nError: %s"
		     file p4-branch-backup-dir copy-result)))

	; Reload the buffer that corresponds to the name of this file, if one exists.
	(save-excursion
	  (set-buffer (find-buffer-visiting local-file))
	  (revert-buffer t t))

        ; Remove all relevant variables from p4 state.
	(p4-state-set files-to-branch-key "")
	(p4-state-set bug-branch-key "")
      )))))

(provide 'my-p4)