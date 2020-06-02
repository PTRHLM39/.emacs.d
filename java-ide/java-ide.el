(setq user-emacs-directory "~/.emacs.d/java-ide/")

(package-initialize)

(setq package-archives '(("melpa" . "http://melpa.org/packages/")
			 ("gnu" . "http://elpa.gnu.org/packages/")))

(require 'cc-mode)

(condition-case nil
    (require 'use-package)
  (file-error
   (require 'package)
   (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
   (package-initialize)
   (package-refresh-contents)
   (package-install 'use-package)
   (require 'use-package)))

;; Let use-package install all packages written if they're not already installed.
(setq use-package-always-ensure t)

;; Store customized variables & faces in seperate file.
(setq custom-file "~/.emacs.d/java-ide/custom.el")
(load custom-file 'noerror)

(defun my-minibuffer-setup-hook ()
  (setq gc-cons-threshold most-positive-fixnum))

(defun my-minibuffer-exit-hook ()
  (setq gc-cons-threshold 800000))

(add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook)
(add-hook 'minibuffer-exit-hook #'my-minibuffer-exit-hook)

;; counsel
(use-package counsel
  :bind
  (("M-x" . counsel-M-x)))

;; IDO
(use-package ido
  :init
  (setq ido-everywhere nil
	ido-enable-flex-matching t
	ido-create-new-buffer 'always
	ido-file-extensionts-order '(".java" ".js" ".el" ".xml") ;suffix
	ido-use-filename-at-point 'guess
	ido-use-faces t)

  :config
  (ido-mode 'buffer)

  :bind ("C-x b" . ido-switch-buffer)
  )

;; flex-matching
(use-package flx-ido)

(use-package ido-vertical-mode
  :init
  (setq ido-vertical-indicator ">>"
	ido-vertical-show-count nil
	ido-vertical-define-key 'C-p-C-p-up-and-down)
  :config
  (ido-vertical-mode)
  (ido-vertical-mode nil))

;; ido support everywhere
(use-package ido-completing-read+
  :config
  (ido-ubiquitous-mode))

;; sub-word support
(add-hook 'minibuffer-setup-hook 'subword-mode)

;; Minibuffer enhancements
(use-package smex)

(use-package ivy
  :init
  (setq ivy-re-builders-alist '((t . ivy--regex-fuzzy))
	ivy-initial-inputs-alist nil)
  :bind
  (("C-x b" . 'ivy-switch-buffer)))

(use-package projectile 
  :ensure t)
(use-package lsp-mode
  :ensure t)
(use-package hydra
  :ensure t)
(use-package lsp-ui
  :ensure t
  :config
  (setq lsp-prefer-flymake nil
	lsp-ui-doc-delay 5.0
	lsp-ui-sideline-enable nil
	lsp-ui-sideline-show-symbol nil))
(use-package lsp-java
  :ensure t
  :config (add-hook 'java-mode-hook 'lsp))

(use-package dap-mode
  :ensure t
  :after lsp-mode
  :config
  (dap-mode t)
  (dap-ui-mode t)
  (dap-tooltip-mode 1)
  (tooltip-mode 1)
  (dap-register-debug-template
   "localhost:5005"
   (list :type "java"
	 :request "attach"
	 :hostName "localhost"
	 :port 5005)))

(use-package dap-java
  :ensure nil
  :after (lsp-java))

(require 'lsp-java-boot)
(add-hook 'lsp-mode-hook #'lsp-lens-mode)
(add-hook 'java-mode-hook #'lsp-java-boot-lens-mode)

(use-package helm
  :init
  (defun ptr-list-buffers()
    (interactive)
    (let ((helm-full-frame t))
      (helm-mini)))
  :bind
  ("C-x C-b" . 'ptr-list-buffers))

(defun close-all-buffers ()
  (interactive)
  (mapc 'kill-buffer (buffer-list)))

(use-package helm-swoop
  :bind
  ("C-\\" . helm-swoop))

(use-package helm-projectile
  :init
  (setq helm-ag-insert-at-point 'symbol)
  :bind
  ("C-'" . helm-projectile-ag))

(show-paren-mode t)
(setq show-paren-style 'expression)

(use-package paren)
(set-face-background 'show-paren-match (face-background 'default))
(set-face-attribute 'show-paren-match nil :weight 'extra-bold)

(use-package yasnippet
  :init
  (setq yas/root-directory '("~/.emacs.d/java-ide/snippets"))
  :config
  (autoload 'yas/expand "yasnippet" t)
  (autoload 'yas/load-directory "yasnippet" t)
  (mapc 'yas/load-directory yas/root-directory)
  (yas-global-mode 1))

(use-package company
  :init
  (setq company-idle-delay 0.0
	company-minimum-prefix-length 1))
(global-company-mode 1)
(global-set-key (kbd "<C-return>") 'company-complete)

(use-package company-lsp
  :commands company-lsp)

(use-package company-emoji)
(add-to-list 'company-backends 'company-emoji)

(use-package realgud
  :config
  (setq realgud-safe-mode nil))

(use-package compile
  :init
  (setq compilation-ask-about-save nil
	compilation-scroll-output 'next-error
	compilation-skip-threshold 2))

(make-variable-buffer-local 'my-compilation-start-time)

(add-hook 'compilation-start-hook #'my-compilation-start-hook)
(defun my-compilation-start-hook (proc)
  (setq my-compilation-start-time (current-time)))

(add-hook 'compilation-finish-functions #'my-compilation-finish-function)
(defun my-compilation-finish-function (buf why)
  (let* ((elapsed (time-subtract nil my-compilation-start-time))
	 (msg (format "Compilation took: %s" (format-time-string "%T.%N" elapsed t))))
    (save-excursion (goto-char (point-max)) (insert msg))
    (message "Compilation %s: %s" (string-trim-right why) msg)))

(use-package flycheck)

(use-package gtags)
(setq xref-prompt-for-identifier nil)

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

(defun config-visit ()
  (interactive)
  (find-file "~/.emacs.d/java-ide/readme.org"))
(global-set-key (kbd "C-c e") 'config-visit)

(use-package which-key
  :ensure t
  :config
  (which-key-mode))

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

(setq scroll-conservatively 100)

;; Scrolling while keeping cursor where it is.
(defun help/scroll-up-one-line ()
  (interactive)
  (scroll-down 1))
(defun help/scroll-down-one-line ()
  (interactive)
  (scroll-up 1))
(global-set-key (kbd "M-p") 'help/scroll-down-one-line)
(global-set-key (kbd "M-n") 'help/scroll-up-one-line)

(when window-system
  (use-package pretty-mode
    :ensure t
    :config
    (global-pretty-mode t)))

(when window-system
 (use-package all-the-icons
  :ensure t))

(use-package cloud-theme
  :ensure t
  :config
  (load-theme 'cloud t))

(use-package lsp-treemacs
  :ensure t
  :config
  (lsp-metals-treeview-enable t)
  (setq lsp-metals-treeview-show-when-views-received t))

(setq line-number-mode t)
(setq column-number-mode t)

(use-package linum-relative
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'linum-relative-mode))
