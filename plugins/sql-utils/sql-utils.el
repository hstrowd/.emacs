;; ---------------- SQL Utilities -------------------

(defun connect-to-psql-db (database-name)
  "Connects to the postgres database specified by sql-user, sql-database, sql-server, 
and product. Creates a new buffer for this connection with the provided database name."
  (let ((buf-name (database-to-buffer-name database-name)))
    ; Connect to database.
    (sql-postgres)
    ; All done.
    (rename-buffer buf-name)
    (pop-to-buffer buf-name)))

(defun connect-to-mysql-db (database-name)
  "Connects to the mysql database specified by sql-user, sql-database, sql-server. 
Creates a new buffer for this connection with the provided buffer name."
  (let ((buf-name (database-to-buffer-name database-name)))
    ; Connect to database.
    (sql-mysql)
    ; All done.
    (rename-buffer buf-name)
    (pop-to-buffer buf-name)))

(defun database-to-buffer-name (database-name)
  "Identifies a unique name for the buffer connecting to the database with the provided
name."
  ; Identify a unique buffer name.
  (if (not (get-buffer database-name))
      database-name
    (setq index 1)
    (while (get-buffer (concat sql-database (number-to-string index)))
      (setq index (1+ index)))
    (concat sql-database (number-to-string index))))

(provide 'sql-utils)