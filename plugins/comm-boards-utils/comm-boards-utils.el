;; ------------- Community Boards Utilities -----------------

;; Database Utilities
(require 'sql)

(add-to-list 'load-path "~/.emacs.d/plugins/sql-utils")
(require 'sql-utils)

(defun comm-boards-connect-to-db ()
  "Establishes a connection to the database"
  (interactive)

  (setq sql-user "root"
	sql-password "admin"
	sql-database "development"
	sql-server "localhost")

  ; Connect to the DB.
  (connect-to-mysql-db sql-database))


(provide 'comm-boards-utils)