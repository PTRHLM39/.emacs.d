;;; init.el --- Small file containing garbage collection, package-archives, bootstraps, asynchronous processing and load-paths.

;;; Commentary:

;;; C-c l to evaluate the config.org

;;; code:

;;; Garbage collection. Makes emacs start up smoother ;;;;;;
(setq gc-cons-threshold 402653184
      gc-cons-percentage 0.6)

(defvar startup/filename-handler-alist file-name-handler-alist
  (setq file-name-handler-alist nil))

(defun startup/revert-file-name-handler-alist ()
  (setq file-name-handler-alist startup/file-name-handler-alist))

(defun startup/reset-gc ()
  (setq gc-cons-threshold 16777216
	gc-cons-percentage 0.1))

(add-hook 'emacs-startup-hook 'startup/revert-file-name-handler-alist)
(add-hook 'emacs-startup-hook 'startup/reset-gc)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
(add-to-list 'load-path "~/.emacs.d/lisp")

;;; init.el ends here
