;; ------------- Community Boards Utilities -----------------

;; Database Utilities
(require 'sql)

(add-to-list 'load-path "~/.emacs.d/plugins/sql-utils")
(require 'sql-utils)


(defun dealrod-connect-to-db ()
  "Establishes a connection to the database"
  (interactive)

  (setq sql-user "dealrod_prep"
	sql-password "dealrod"
	sql-database "dealrod_prep"
	sql-server "localhost")

  ; Connect to the DB.
  (connect-to-mysql-db sql-database))


(provide 'dealrod-utils)