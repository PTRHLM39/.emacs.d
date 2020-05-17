(defun config-visit ()
  (interactive)
  (find-file "~/.emacs.d/config.org"))
(global-set-key (kbd "C-c e") 'config-visit)

(defun stumpwm-visit ()
  (interactive)
  (find-file "~/.stumpwm.d/readme.org"))
(global-set-key (kbd "C-c s") 'stumpwm-visit)

(defun bash-visit ()
  (interactive)
  (find-file "~/.bashrc.org"))
(global-set-key (kbd "C-c b") 'bash-visit)

(defun config-eval ()
  "Evaluating ~/.emacs.d/config.org at runtime"
  (interactive)
  (org-babel-load-file (expand-file-name "~/.emacs.d/config.org")))
(global-set-key (kbd "C-c l") 'config-eval)

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

(use-package which-key
  :ensure t
  :config
  (which-key-mode))

(setq line-number-mode t)
(setq column-number-mode t)

(use-package linum-relative
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'linum-relative-mode))

(use-package sudo-edit
  :ensure t
  :bind
  ("s-e" . sudo-edit))

(use-package smartparens
  :ensure t
  :config (require 'smartparens-config)
  (show-smartparens-global-mode 1)
  (smartparens-global-mode 1)
  (sp-use-paredit-bindings))

(use-package mark-multiple
  :ensure t
  :bind ("C-c q" . 'mark-next-like-this))

(use-package expand-region
  :ensure t
  :bind ("C-q" . er/expand-region))

(use-package hungry-delete
  :ensure t
  :config
  (global-hungry-delete-mode))

(use-package zzz-to-char
  :ensure t
  :bind ("M-z" . zzz-up-to-char))

(use-package switch-window
  :ensure t
  :config
  (setq switch-window-input-style 'minibuffer)
  (setq switch-window-increace 4)
  (setq switch-window-threshold 2)
  (setq switch-window-shortcut-style 'qwerty)
  (setq switch-window-qwerty-shortcuts
	'("a" "s" "d" "f" "j" "k" "l" "i" "o"))
  :bind
  ([remap other-window] . switch-window))

(use-package avy
  :ensure t
  :bind ("M-s" . avy-goto-word-1))

(defun split-and-follow-horizontally ()
  (interactive)
  (split-window-below)
  (balance-windows)
  (other-window 1))
(global-set-key (kbd "C-x 2") 'split-and-follow-horizontally)

(defun split-and-follow-vertically ()
  (interactive)
  (split-window-right)
  (balance-windows)
  (other-window 1))
(global-set-key (kbd "C-x 3") 'split-and-follow-vertically)

(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)

(defvar my-term-shell "/bin/bash")
(defadvice ansi-term (before force-bash)
  (interactive (list my-term-shell)))
(ad-activate 'ansi-term)

(global-set-key (kbd "C-.") 'ansi-term)

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

;;(setq display-time-24hr-format t)
;;(setq display-time-format "%H:%M - %d %B %Y")
;;(display-time-mode 1)

;;(use-package fancy-battery
;;  :ensure t
;;  :config
;;  (setq fancy-battery-show-percentage t)
;;  (setq battery-update-interval 15)
;;  (if window-system
;;      (fancy-battery-mode)
;;      (display-battery-mode)))

(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode))

(when window-system
 (use-package all-the-icons
  :ensure t))

(use-package monokai-theme
  :ensure t
  :config
  (load-theme 'monokai t))

(setq scroll-conservatively 100)

(defalias 'yes-or-no-p 'y-or-n-p)

(when window-system
  (use-package pretty-mode
    :ensure t
    :config
    (global-pretty-mode t)))

(use-package slime
    :ensure t
    :config
    (setq inferior-lisp-program "/usr/bin/sbcl")
    (setq slime-contribs '(slime-fancy)))

(add-hook 'slime-load-hook
	   (lambda ()
	     (define-key slime-prefix-map (kbd "M-h")'slime-documentation-lookup)))

(setq slime-lisp-implementations
      '((sbcl ("sbcl" "--core" "sbcl.core-for-slime"))))

(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
(add-hook 'emacs-lisp-mode-hook 'yas-minor-mode)
(add-hook 'emacs-lisp-mode-hook 'company-mode)

(use-package slime-company
  :ensure t
  :init
  (require 'company)
  (slime-setup '(slime-fancy slime-company)))

(use-package highlight-defined
  :ensure t
  :config
  (add-hook 'emacs-lisp-mode-hook 'highlight-defined-mode))

(use-package magit
  :ensure t
  :config
  (setq magit-push-always-verify nil)
  (setq git-commit-summary-max-length 50)
  :bind
  ("M-g" . magit-status))

(use-package yasnippet
  :ensure t
  :init
  (yas-global-mode 1))

(use-package yasnippet-snippets
  :ensure t)

(use-package go-snippets
  :ensure t)

(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode t)
  (defun disable-flycheck-in-org-src-block ()
    "Disables flychecks that could be problematic in org-mode"
    (setq-local flycheck-disabled-checkers '(emacs-lisp-checkdoc)))
  :hook ((prog-mode . flycheck-mode)
	 (org-src-mode . disable-flycheck-in-org-src-block)))

(use-package company
  :ensure t
  :config
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 3)
  (global-company-mode t)

(custom-set-faces
 '(company-preview
   ((t (:foreground "darkgray" :underline t))))
 '(company-preview-common
   ((t (:inherit company-preview :weight bold))))
 '(company-tooltip
   ((t (:inherit popup-face))))
 '(company-tooltip-selection
   ((t (:inherit popup-menu-selection-face))))
 '(company-tooltip-common
   ((((type x)) (:inherit company-tooltip))))
 '(company-tooltip-common-selection
   ((((type x)) (:inherit company-tooltip-selection :weight bold))
    (t (:inherit company-tooltip-selection))))))

(add-hook 'shell-mode-hook 'yas-minor-mode)
(add-hook 'shell-mode-hook 'flycheck-mode)
(add-hook 'shell-mode-hook 'company-mode)

(defun shell-mode-company-init ()
  (setq-local company-backends '((company-shell
				  company-shell-env
				  company-etags
				  company-dabbrev-code))))

(use-package company-shell
:ensure t
:config
(require 'company)
(add-hook 'shell-mode-hook 'shell-mode-company-init))

(use-package go-mode
  :ensure t
  :config
  (add-to-list 'load-path "~/go/src/github.com/dougm/goflymake")
  (require 'go-flymake))

(use-package go-playground
  :ensure t)

(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :hook (go-mode . lsp-deferred))

(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode)

(use-package company-lsp
  :ensure t
  :commands company-lsp)

(use-package org
  :ensure t
  :pin org
  :config
  (org-babel-do-load-languages 'org-babel-load-languages
			       '(
				 (shell .t)
				 )))

(load "auctex.el" nil t t)
(load "preview-latex.el" nil t t)

(setq TeX-auto-save  t
      TeX-parse-self t)
(setq-default TeX-master nil)

(setq LaTeX-section-hook
		'(LaTeX-section-heading
		LaTeX-section-title
		LaTeX-section-toc
		LaTeX-section-section
		LaTeX-section-label))
