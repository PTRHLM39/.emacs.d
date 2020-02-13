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
