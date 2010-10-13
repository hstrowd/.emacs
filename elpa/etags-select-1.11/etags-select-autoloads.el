;;; etags-select-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (etags-select-find-tag etags-select-find-tag-at-point
;;;;;;  etags-select-mode-hook etags-select-no-select-for-one-match
;;;;;;  etags-select-mode) "etags-select" "etags-select.el" (19599
;;;;;;  25551))
;;; Generated autoloads from etags-select.el

(let ((loads (get (quote etags-select-mode) (quote custom-loads)))) (if (member (quote "etags-select") loads) nil (put (quote etags-select-mode) (quote custom-loads) (cons (quote "etags-select") loads))))

(defvar etags-select-no-select-for-one-match t "\
*If non-nil, don't open the selection window if there is only one
matching tag.")

(custom-autoload (quote etags-select-no-select-for-one-match) "etags-select" t)

(defvar etags-select-mode-hook nil "\
*List of functions to call on entry to etags-select-mode mode.")

(custom-autoload (quote etags-select-mode-hook) "etags-select" t)

(autoload (quote etags-select-find-tag-at-point) "etags-select" "\
Do a find-tag-at-point, and display all exact matches.  If only one match is
found, see the `etags-select-no-select-for-one-match' variable to decide what
to do.

\(fn)" t nil)

(autoload (quote etags-select-find-tag) "etags-select" "\
Do a find-tag, and display all exact matches.  If only one match is
found, see the `etags-select-no-select-for-one-match' variable to decide what
to do.

\(fn)" t nil)

;;;***

;;;### (autoloads nil nil ("etags-select-pkg.el") (19599 25551 174886))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; etags-select-autoloads.el ends here
