;; clojure
;;(defun turn-on-paredit () (paredit-mode 1))
;;(add-hook 'clojure-mode-hook 'turn-on-paredit)

;; rainbow-delimiters in all programming modes
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;; php
(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))

;; ActionScript
(add-to-list 'auto-mode-alist '("\\.as$" . actionscript-mode))

;; Java
(add-hook 'java-mode-hook (lambda ()
          (setq c-basic-offset 2
          tab-width 2
          indent-tab-mode nil)))

;; Ruby
(add-to-list 'auto-mode-alist '("Gemfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec$" . ruby-mode))

;; ClojureScript
(add-to-list 'auto-mode-alist '("\\.cljs$" . clojure-mode))

;; YAML
(add-to-list 'auto-mode-alist '("\\.conf.tmpl$" . yaml-mode))

;; Markdown
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))

;; Go
(if (file-exists-p "/usr/local/opt/go/misc/emacs/go-mode-load.el")
    (progn
      (setq load-path (cons "/usr/local/opt/go/misc/emacs" load-path))
      (require 'go-mode-load)
      (add-to-list 'auto-mode-alist '("\\.go$" . go-mode))))

;; Zsh
(add-to-list 'auto-mode-alist '("\\.zsh$" . sh-mode))

;; cleanup trailing whitespace
(setq whitespace-action '(auto-cleanup)) ;; automatically clean up bad whitespace
(setq whitespace-style '(trailing space-before-tab indentation empty space-after-tab)) ;; only show bad whitespace
(global-whitespace-mode t)

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward
      uniquify-after-kill-buffer-p t ; reorganize buffer names after a buffer has been killed
      uniquify-ignore-buffers-re "^\\*")

;; elixir stuff
(require 'elixir-mode-setup)
(elixir-mode-setup)
