;;;; init.el --- Small file containing garbage collection, package-archives, bootstraps, asynchronous processing and load-paths.

;;; Commentary:

;;; C-c l to evaluate the config.org

;;; code:

(require 'package)

;;; Melpa, elpa & org package-archive

(setq package-enable-at-startup nil)

(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))

(add-to-list 'package-archives
	     '("gnu" . "https://elpa.gnu.org/packages/"))

(add-to-list 'package-archives
	     '("org" . "https://orgmode.org/elpa/"))

(package-initialize)

;; Bootstrap `use-package

(unless (package-installed-p 'use-package)
(package-refresh-contents)
(package-install 'use-package))


(setq select-enable-clipboard t)

;; Asynchronous processing
;; dired
(autoload 'dired-async-mode "dired-async.el" nil t)
(dired-async-mode 1)

;; Load-path
(org-babel-load-file (expand-file-name "~/.emacs.d/config.org"))
(add-to-list 'load-path "~/.emacs.d/lisp/")

;; SBCL
;; quicklisp in slime-repl
(load (expand-file-name "~/quicklisp/slime-helper.el"))
  (setq inferior-lisp-program "sbcl")

(require 'aweshell)

;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (monokai-theme zzz-to-char yaxception yasnippet-snippets which-key use-package treemacs switch-window sudo-edit smartparens slime-company rich-minority pretty-mode nord-theme mark-multiple magit lsp-ui log4e linum-relative hungry-delete highlight-defined go-snippets go-playground flycheck-clang-analyzer fancy-battery expand-region doom-modeline dired-narrow company-shell company-lsp company-irony company-c-headers autothemer anaphora))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-preview ((t (:foreground "darkgray" :underline t))))
 '(company-preview-common ((t (:inherit company-preview :weight bold))))
 '(company-tooltip ((t (:inherit popup-face))))
 '(company-tooltip-common ((((type x)) (:inherit company-tooltip))))
 '(company-tooltip-common-selection ((((type x)) (:inherit company-tooltip-selection :weight bold)) (t (:inherit company-tooltip-selection))))
 '(company-tooltip-selection ((t (:inherit popup-menu-selection-face)))))
