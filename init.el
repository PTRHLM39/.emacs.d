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

;; Load-path
(org-babel-load-file (expand-file-name "~/.emacs.d/config.org"))
(add-to-list 'load-path "~/.emacs.d/lisp/")

;; SBCL
;; quicklisp in slime-repl
(load (expand-file-name "~/quicklisp/slime-helper.el"))
  (setq inferior-lisp-program "sbcl")

;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(delete-selection-mode nil)
 '(package-selected-packages
   (quote
    (transient zzz-to-char yasnippet-snippets which-key use-package switch-window sudo-edit smartparens slime-company pretty-mode mark-multiple magit linum-relative kaolin-themes hungry-delete highlight-defined helm go-snippets go-playground flycheck expand-region elpy doom-modeline company-shell))))
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
