;; ------------ Plugin Variables --------------

(setq p4-tmp-buf-name "*p4-tmp*")
(setq p4-state-file "/home/cnuapp/.emacs.d/plugins/my-p4/p4.state")
(setq p4-chglst-tmplt "/home/cnuapp/.emacs.d/plugins/my-p4/p4-init-changelist")
(setq p4-chglst-file-prefix "/home/cnuapp/.emacs.d/plugins/my-p4/changelist-specs/chnglst.")
(setq p4-chglst-file-postfix ".log")


;; ------------ Helper Functions --------------

(defun convert-web-to-comp-stable (file-path)
  "Converts a file in /export/web/... to /export/comp/stable/..."
  (if (equal (substring file-path 0 12) "/export/web/")
      (concat "/export/comp/stable/" (substring file-path 12))
    file-path))

(defun clear-buffer (buffer-or-name)
  "Deletes all content from the provided buffer"
  (interactive "bBuffer: ")
  (let ((buffer (get-buffer buffer-or-name)))
    (if buffer
	(save-excursion
	  (set-buffer buffer)
	  (erase-buffer))
      (error "Specified buffer does not exist"))))

(defun p4-get-depot-path (local-file)
  "Identifies the full depot path of the latest revision synced of the provided file."
  (let ((p4-buf (get-buffer-create p4-tmp-buf-name)))
    ; Clear the p4 output buffer.
    (clear-buffer p4-buf)

    ; Run a have command.
    (call-process "p4" nil p4-tmp-buf-name nil "have" local-file)
      (save-excursion
	(set-buffer p4-buf)
	(save-excursion
	  ; Parse the full depot path out of the result.
	  (goto-char (point-max))
	  (if (re-search-backward "#[0-9]+ - /export/comp/stable/" nil t)
	      (progn
		(set-mark (point))
		(message "Success: found the later part of have.")
		(if (re-search-backward "^//depot/cnuapp/" nil t)
		    (buffer-substring (mark) (point))
		  (error "Failed: Unable to identify the latest revision synced of: %s" local-file)))
	    (error "Failed: Unable to identify the latest revision synced of : %s" local-file))))))


;; ---------- Interactive Functions -----------

(defun p4-check-out ()
  "Check out the file corresponding to the current buffer.
An error is thrown in any of the following circumstances:
  - The user is not logged into p4.
  - The current buffer does not correspond to a file.
  - The file corresponding to the current buffer is not in the p4 client."
  (interactive)
  (message "Starting check out")
  (let ((local-file (convert-web-to-comp-stable (buffer-file-name)))
	(cur-buf (current-buffer))
	(p4-buf (get-buffer-create p4-tmp-buf-name)))
    (message "local-file: %s" (buffer-file-name))
    (message "local-file: %s, cur-buf: %s, p4-buf: %s" local-file cur-buf p4-buf)
    ; Clear the p4 output buffer.
    (clear-buffer p4-buf)
    ; Attempt to check-out the file
    (call-process "p4" nil p4-tmp-buf-name nil "edit" local-file)
    (save-excursion
      (set-buffer p4-buf)
      (save-excursion
	(goto-char (point-min))
	(if (re-search-forward "#[0-9]+ - opened for edit" nil t)
	    ; If successful, make the buffer read-only.
	    (progn 
	      (set-buffer cur-buf)
	      (setq buffer-read-only nil)
	      (message "Success: Checked out %s" local-file))
	  ; If unsuccessful, throw an error.
	  (error "Failed: Unable to check out %s" local-file))))))

(defun p4-rebase ()
  "Rebases the current file by integrating any changes from comp/stable"
  (interactive)
  (message "Starting rebase")
  (let ((local-file (convert-web-to-comp-stable (buffer-file-name)))
	(cur-buf (current-buffer))
	(p4-buf (get-buffer-create p4-tmp-buf-name)))
    (message "local-file: %s, cur-buf: %s, p4-buf: %s" local-file cur-buf p4-buf)
    ; Clear the p4 output buffer.
    (clear-buffer p4-buf)

    ; Identify the currently synced revision and the comp/stable revision
    ; Attempt to integrate the corresponding file from comp/stable
    (let ((cur-sync-depot-file (p4-get-depot-path local-file))
	  (comp-stable-depot-file (format "//depot/cnuapp/%s" (substring local-file 8))))
      (call-process "p4" nil p4-tmp-buf-name nil "integrate" "-3" comp-stable-depot-file cur-sync-depot-file)
      (save-excursion
	(set-buffer p4-buf)
	(save-excursion
	  (goto-char (point-min))
	  ; TODO: figure out what the proper string is here.
	  (if (re-search-forward (format "%s#[0-9]+ - integrate from %s#[0-9]+" cur-sync-depot-file comp-stable-depot-file) nil t)
	      (message "Success: %s has been integrated into %s. Please resolve any conflicts." comp-stable-depot-file cur-sync-depot-file)
	    (if (re-search-forward " - all revision(s) already integrated." nil t)
		(message "All revision(s) already integrated.")
	      (error "Failed: Unable to integrate from %s to %s" comp-stable-depot-file cur-sync-depot-file))))))))




(provide 'my-p4)



;; ---------- Work In Progress -----------

(defun p4-br-cur-edit (branch)
  "Branches the file corresponding to the current buffer onto the specified branch."
  (interactive "MEnter the branch to which this should be integrated: ")
  (p4-branch-edit-to-bug-br  (convert-web-to-comp-stable (buffer-file-name)) 
			     branch))


; Creating a new p4 changelist
(defun p4-start-change (issue-number source-branch)
  "Creates a new changelist for branching files from SOURCE-BRANCH to bug/ISSUE-NUMBER_sparse branch."
  (interactive "nIssue Number: \nMSource Branch:")
  (p4-create-changelist p4-client-name p4-user-name (format "%d_sparse" issue-number) source-branch issue-number))

(defun replace-all (text replacement)
  "Replaces all instances of the TEXT with the REPLACEMENT in the current buffer."
  (goto-char (point-min))
  (while (re-search-forward text nil t)
    (replace-match replacement nil t)))

(defun p4-create-changelist (client-name user-name bug-branch source-branch issue-number)
  "Creates a new changelist to be used by the my-p4 plugin.
If one already exist, an error is thrown."
  (let ((p4-buf (get-buffer-create p4-tmp-buf-name))
	(p4-chglst-buffer (find-file-noselect p4-chglst-tmplt))
	(p4-chglst-file (concat p4-chglst-file-prefix 
				(concat (format-time-string "%Y%m%d%H%M%S") 
					p4-chglst-file-postfix)))
	(p4-state-buffer (find-file-noselect p4-state-file)))
    ; Clear the p4 output buffer.
    (clear-buffer p4-buf)
    (save-excursion 
      ; Identify the current changelist
      (set-buffer p4-state-buffer)
      (save-excursion
	(goto-char (point-min))
	(re-search-forward " -- current_changelist: ")
	(set-mark (point))
	(if (re-search-forward "[0-9]+" nil t)
	    (error "The my-p4 changelist is already set to: %s" (buffer-substring (mark) (point))))
	; Create the initial changelist.
	(set-buffer p4-chglst-buffer)
	(replace-all "<client-name>" client-name)
	(replace-all "<user-name>" user-name)
	(replace-all "<bug-branch>" bug-branch)
	(replace-all "<source-branch>" source-branch)
	(replace-all "<issue-number>" issue-number)
	(write-region (point-min) (point-max) p4-chglst-file nil nil nil t)

	(call-process "p4" p4-init-changelist p4-tmp-buf-name nil "change" "-i")
	(set-buffer p4-buf)
	(save-excursion 
	  (goto-char (point-max))
	  (re-search-backward "Change [0-9]+ created." nil t)
	  (goto-char (+ (point) 7))
	  (set-mark (point))
	  (re-search-forward "[0-9]+")
	  (let ((changelist-num (buffer-substring (mark) (point)))
		(start (mark))
		(end (point)))
	    (set-buffer p4-state-buffer)
	    (insert-buffer-substring p4-buf start end)
	    changelist-num))))))

(defun p4-get-changelist ()
  "Identifies the current changelist being used by the my-p4 plugin.
If one does not exist, an error will be thrown."
  (let ((p4-buf (get-buffer-create p4-tmp-buf-name))
	;(p4-changelist-buffer (find-file-noselect p4-init-changelist))
	(p4-state-buffer (find-file-noselect p4-state-file)))
    ; Clear the p4 output buffer.
    (clear-buffer p4-buf)
    (save-excursion 
      ; Identify the current changelist
      (set-buffer p4-state-buffer)
      (save-excursion
	(goto-char (point-min))
	(re-search-forward " -- current_changelist: ")
	(set-mark (point))
	(if (re-search-forward "[0-9]+" nil t)
	    (let ((chglst (buffer-substring (mark) (point))))
	      chglst)
	  (error "No existing my-p4 changelist."))))))

;(defun p4-show-chglst ()
;  "Prints the current changelist being used by the my-p4 plugin."
;  (interactive)
;  (message (p4-get-changelist)))




(defun p4-branch-edit-to-bug-br (local-file branch)
  "Branch a file from the local client that is checked out to a specified bug branch in the depot."
  (let ((p4-buf (get-buffer-create p4-tmp-buf-name))
	(changelist (p4-get-changelist-create)))
    (clear-buffer p4-tmp-buf-name)
    (save-excursion 
      (set-buffer p4-buf)
      (call-process "p4" nil p4-tmp-buf-name nil "have" local-file)
      (save-excursion
	(goto-char (point-max))
	(re-search-backward "#[0-9]+ - \\/" nil t)
	(set-mark (point))
	(re-search-forward "#[0-9]+ - \\/" nil t)
	(let ((depot-file (buffer-substring 1 (mark)))
	      (branch-indep-file (buffer-substring (+ (point) 19) (- (point-max) 1))))
	  (message "The depot file is: %s\nThe branch independent file is: %s" depot-file branch-indep-file)
	  (call-process "p4" nil p4-tmp-buf-name nil "revert" depot-file)
	  (call-process "p4" nil p4-tmp-buf-name nil "sync" (format "%s#none" depot-file))
	  (call-process "p4" nil p4-tmp-buf-name nil "sync" (format "//depot/cnuapp/bug/%s/%s" branch branch-indep-file))
	  (call-process "p4" nil p4-tmp-buf-name nil "integrate" "-3c" depot-file (format "//depot/cnuapp/bug/%s/%s" branch branch-indep-file)))))))




(defun p4-branch-have (local_file branch)
  "Branch a file from the local client to a specified bug branch."
  (let ((file (get-path-from-comp-stable (current-buffer))))
    (let ((depot-file (format "//depot/cnuapp/comp/stable/%s" file)))
      (message "The depot-file for current buffer is %s" depot-file)
;      (call_process "p4" nil "*Messages*" nil "opened")
;      (call-process "p4" nil "*Messages*" nil "sync" depot-file)
      (call-process "p4" nil "*Messages*" nil "have" depot-file)
      (call-process "p4" nil "*Messages*" nil "sync" (format "%s#none" depot-file))
      (call-process "p4" nil "*Messages*" nil "integrate" "-3" depot-file (format "//depot/cnuapp/bug/%s/%s" branch file)))))
  ;(call-process "p4 have //depot/cnuapp/comp/stable/%s"))
;(or (equal (substring local-file 0 20) "/export/comp/stable/") ...)

