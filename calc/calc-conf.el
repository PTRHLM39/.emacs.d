(add-to-list 'load-path "~/.emacs.d/lisp/")

(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)

;; Disable startup-message
(setq inhibit-startup-message t)

;; Disable tool-bar
(tool-bar-mode -1)

;; Disable menu-bar
(menu-bar-mode -1)

;; Disable Scroll-bar
(scroll-bar-mode -1)

;; Disable bell
(setq ring-bell-function 'ignore)

;; Disable backups and auto-save-files
(setq make-backup-files nil)
(setq quto-save-default nil)

;; Save session
;;(desktop-save-mode 1)

(require 'helm)
(require 'helm-config)

(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))

(define-key helm-map (kbd "C-z") 'helm-select-action)

(setq helm-split-window-in-side-p           t
      helm-move-to-line-cycle-in-source     t
      helm-ff-search-library-in-sexp        t
      helm-ff-file-name-history-use-recentf t)

(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-s") 'helm-occur-from-isearch)
(global-set-key (kbd "C-x /") 'helm-find)
