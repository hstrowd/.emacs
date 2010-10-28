;; to find unbalanced parens -- go to the end of the file and type C-u C-M-u.
;; This will move you to the beginning of the first defun that is unbalanced.

(setq major-mode 'text-mode)

;; Turn on paren matching
(show-paren-mode t)
(setq show-paren-style 'mixed)

;; Get rid of the startup screen
(setq inhibit-startup-screen t)
(setq initil-scratch-message nil)

;; maxframe
(add-to-list  'load-path "~/.emacs.d/plugins/maxframe")
(require 'maxframe)
(add-hook 'window-setup-hook 'maximize-frame t)

;; Get back font antialiasing
(push '(font-backend xft x) default-frame-alist)

;; Get rid of toolbar, scrollbar, menubar
(progn
  (tool-bar-mode)
;  (menu-bar-mode)
  (scroll-bar-mode))

;; Use the TextMate plugin
;; TextMate automatically completes characters like ', ", [, {, or (
;; This is very annoying.
;(add-to-list 'load-path "~/.emacs.d/plugins/textmate")
;(require 'textmate)
;(textmate-mode)

;; Use the Redo plugin
(add-to-list 'load-path "~/.emacs.d/plugins/redo")
(require 'redo)
(global-set-key [(control -)] 'redo)

;; Centers the screen around a line
(global-set-key [(control l)] 'centerer)
(defun centerer ()
  "Repositions current line: once middle, twice top, thrice bottom"
  (interactive)
  (cond ((eq last-command 'centerer2) ; 3 times pressed = bottom
         (recenter -1))
        ((eq last-command 'centerer1) ; 2 times pressed = top
         (recenter 0)
         (setq this-command 'centerer2))
        (t
         (recenter)
         (setq this-command 'centerer1))))

;; Set the color-scheme
(set-background-color "#2b2b2b")
(set-foreground-color "white")
(set-face-background 'modeline "DarkRed")
(set-face-foreground 'modeline "white")

;; Use the LineNum plugin
;; This just simply didn't work for me.
;(add-to-list 'load-path "~/.emacs.d/plugins/linenum")
;(autoload'linum-mode "linum" "togle line numbers on/off")
;(global-set-key (kbd "C-<f5>") 'linum-mode)

;; Use the find-recursive plugn
(add-to-list 'load-path "~/.emacs.d/plugins/find-recursive")
(require 'find-recursive)

;; Use the Anything plugin
(add-to-list 'load-path "~/.emacs.d/plugins/anything")
(require 'anything)

;; Use the RCodeTools plugin
;; This may require the installation of the rcodetools gem
;; See http://emacsblog.org/2007/07/21/package-faves-rcodetools/ for instructions
;(add-to-list 'load-path "~/.emacs.d/plugins/rcodetools")
;(require 'rcodetools)

;; Interactively Do Things (highly recommended, but not strictly required)
(require 'ido)
(ido-mode t)

;; Use css-mode
(add-to-list  'load-path "~/.emacs.d/plugins/css-mode")
(autoload 'css-mode "css-mode" "Mode for editing CSS files" t)
(setq auto-mode-alist (append '(("\\.css$" . css-mode)) auto-mode-alist))
(add-hook 'css-mode-hook
          (lambda()
            (local-set-key (kbd "<return>") 'newline-and-indent)))

;; Use ruby-mode
(add-to-list 'load-path "~/.emacs.d/plugins/ruby-mode")
(require 'ruby-mode)
(require 'ruby-electric)
(add-hook 'ruby-mode-hook 'turn-on-font-lock)
(add-to-list 'auto-mode-alist '("\\.rjs$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))

;; Use ruby-block plugin
(add-to-list 'load-path "~/.emacs.d/plugins/ruby-block")
(require 'ruby-block)

;; Use yaml-mode
(add-to-list 'load-path "~/.emacs.d/plugins/yaml-mode")
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

;; Use the flymake plugin
;(add-to-list  'load-path "~/.emacs.d/plugins/flymake")
;(require 'flymake)

;; Change the default color-scheme
;(set-face-background 'flymake-errline "red4")
;(set-face-background 'flymake-warnline "dark slate blue")


;; (defun flymake-create-temp-intemp (file-name prefix)
;;   "Return file name in temporary directory for checking FILE-NAME.
;; This is a replacement for `flymake-create-temp-inplace'. The
;; difference is that it gives a file name in
;; `temporary-file-directory' instead of the same directory as
;; FILE-NAME.

;; For the use of PREFIX see that function.

;; Note that not making the temporary file in another directory
;; \(like here) will not if the file you are checking depends on
;; relative paths to other files \(for the type of checks flymake
;; makes)."
;;   (unless (stringp file-name)
;;     (error "Invalid file-name"))
;;   (or prefix
;;       (setq prefix "flymake"))
;;   (let* ((name (concat
;;                 (file-name-nondirectory
;;                   (file-name-sans-extension file-name))
;;                 "_" prefix))
;;          (ext  (concat "." (file-name-extension file-name)))
;;          (temp-name (make-temp-file name nil ext))
;;          )
;;     (flymake-log 3 "create-temp-intemp: file=%s temp=%s" file-name temp-name)
;;     temp-name))

;; Invoke ruby with '-c' to get syntax checking
;; (defun flymake-ruby-init ()
;;   (let* ((temp-file   (flymake-init-create-temp-buffer-copy
;;                        'flymake-create-temp-intemp))
;;          (local-file  (file-relative-name
;;                        temp-file
;;                        (file-name-directory buffer-file-name))))
;;     (list "ruby" (list "-c" local-file))))

;; (push '(".+\\.rb$" flymake-ruby-init) flymake-allowed-file-name-masks)
;; (push '(".+\\.rjs$" flymake-ruby-init) flymake-allowed-file-name-masks)
;; (push '("Rakefile$" flymake-ruby-init) flymake-allowed-file-name-masks)

;; (push '("^\\(.*\\):\\([0-9]+\\): \\(.*\\)$" 1 2 nil 3) flymake-err-line-patterns)

;; (add-hook 'ruby-mode-hook
;;           '(lambda ()

;;              ;; Disable flymake mode for ruby regions in rhtml files and read only files
;;              (if (and (not (null buffer-file-name)) (file-writable-p buffer-file-name))
;;                  (flymake-mode))
;;              ))

;; Use the Rinari plugin
;(add-to-list 'load-path "~/.emacs.d/plugins/rinari")
;(require 'rinari)
;(setq rinari-tags-file-name "TAGS")


;; Include the snippet plugin
;(add-to-list 'load-path "~/.emacs.d/plugins/snippet")
;(require 'snippet)

;; Use the yasnippet plugin
;(add-to-list 'load-path "~/.emacs.d/plugins/yasnippet")
;(require 'yasnippet)
;(yas/initialize)
;(yas/load-directory "~/.emacs.d/plugins/yasnippet/snippets")
;(setq require-final-newline nil)

;; Enable yasnippet rails
;(load "~/.emacs.d/plugins/yasnippets-rails/setup.el")

;; Use the AutoTest plugin
;(add-to-list 'load-path "~/.emacs.d/plugins/autotest")
;(require 'autotest)

;; Use rhtml-mode
(add-to-list 'load-path "~/.emacs.d/plugins/rhtml")
(require 'rhtml-mode)
;(add-hook 'rhtml-mode-hook
;  (lambda () (rinari-launch)))

;(add-hook 'rhtml-mode
;          (let ((original-command (lookup-key rhtml-mode-map [tab])))
;            `(lambda ()
;               (setq yas/fallback-behavior
;                     '(apply ,original-command))
;               (local-set-key [tab] 'yas/expand))))

(add-to-list 'auto-mode-alist '("\\.rhtml$" . rhtml-mode))

;(add-to-list 'load-path "~/.emacs.d/plugins/auto-complete")
;(require 'auto-complete-config)
;(global-auto-complete-mode t)



;; Start completion when entered 3 characters
;(setq ac-auto-start 2)

;; Define keystrokes for auto-completion
;(define-key ac-complete-mode-map "\t" 'ac-complete)
;(define-key ac-complete-mode-map "\r" nil)
;(define-key ac-complete-mode-map "\C-'" 'ac-previous)
;(define-key ac-complete-mode-map "\C-," 'ac-next)

;(global-auto-complete-mode t)           ;enable global-mode
;(setq ac-auto-start t)                  ;automatically start
;(setq ac-dwim 3)                        ;Do what i mean
;(setq ac-override-local-map nil)        ;don't override local map

;(set-default 'ac-sources '(ac-source-yasnippet ac-source-abbrev ac-source-words-in-buffer))

;(setq ac-modes
;      (append ac-modes
;              '(eshell-mode
;                ;org-mode
;                )))
;(add-to-list 'ac-trigger-commands 'org-self-insert-command)

;(add-hook 'emacs-lisp-mode-hook
;          (lambda ()
;            (setq ac-sources '(ac-source-yasnippet ac-source-abbrev ac-source-words-in-buffer ac-source-symbols))))

;(add-hook 'eshell-mode-hook
;          (lambda ()
;            (setq ac-sources '(ac-source-yasnippet ac-source-abbrev ac-source-files-in-current-dir ac-source-words-in-buffer))))

;(add-hook 'ruby-mode-hook
;          (lambda ()
;            (setq ac-omni-completion-sources '(("\\.\\=" ac-source-rcodetools)))));)

;; Use the rails-emacs plugin
;(add-to-list 'load-path "~/.emacs.d/plugins/emacs-rails")
;(require 'rails)

(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#2b2b2b" :foreground "white" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 101 :width normal :foundry "unknown" :family "DejaVu Sans Mono")))))

(global-set-key (kbd "C-;") 'Control-X-prefix)

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(show-paren-mode t)
 '(transient-mark-mode t))


;;; This was installed by package-install.el.
;;; This provides support for the package system and
;;; interfacing with ELPA, the package archive.
;;; Move this code earlier if you want to reference
;;; packages in your .emacs.
(when
    (load
     (expand-file-name "~/.emacs.d/elpa/package.el"))
  (package-initialize))

;; Highlight marked regions.
;(transient-mark-mode)

;; Properly formats emacs shell
(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook
	  'ansi-color-for-comint-mode-on)



;; Use the p4 plugin I created
;; Required settings.
(setq p4-user-name "hstrowd")
(setq p4-client-name "hstrowd.cnuapp.dev")

;; Load the plugin
(add-to-list 'load-path "~/.emacs.d/plugins/my-p4")
(require 'my-p4)

(global-set-key (kbd "M-p M-c") 'p4-check-out)



;; CNU Utilities
(defun cnu-change-env (country)
  "Changes the symlink for the /etc/cnu/cnu_env file to point
to the requested country"
  (interactive "MCountry: ")
  (let ((country-file (format "/etc/cnu/cnu_env.%s" (upcase country))))
    (if (file-exists-p country-file)
	(call-process "ln" nil "*Message*" nil "-fs" country-file "/etc/cnu/cnu_env")
      (error "File '%s' does not exist." country-file))))

(define-prefix-command 'cnu-command)
(global-set-key (kbd "M-c") 'cnu-command)
(global-set-key (kbd "M-c M-e") 'cnu-change-env)