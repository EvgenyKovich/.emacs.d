;;; begin initialization

;; turn off mouse interface early in startup to avoid momentary display
(when window-system
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (tooltip-mode -1))

;; don't display default text
(setq inhibit-startup-message t
      initial-scratch-message "")

;; setup package
(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives
	     '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives
	     '("org" . "http://orgmode.org/elpa/") t)
(package-initialize)

;; bootstrap use-package
;; install use-package if it's not already installed.
;; use-package is used to configure the rest of the packages.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; from use-package README
(eval-when-compile
  (require 'use-package))
(require 'diminish) ;; if you use :diminish
(require 'bind-key)

;; start emacs server
(server-start)

;; emacs-mac specific stuff
;; don't remember why i have it though and couldn't find any reference to it in the emacs-mac README anymore
(let ((default-directory "/usr/local/share/emacs/site-lisp/"))
  (normal-top-level-add-subdirs-to-load-path))

;; fix deprecation warnings in Emacs 25
(when (>= emacs-major-version 25)
  (eval-after-load 'bytecomp
    '(add-to-list 'byte-compile-not-obsolete-funcs
                  'preceding-sexp)))

;; load the config
(org-babel-load-file (concat user-emacs-directory "configuration.org"))
