
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
(add-to-list 'auto-mode-alist '("Gemfile" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile" . ruby-mode))

;; cleanup trailing whitespace
(setq whitespace-action '(auto-cleanup)) ;; automatically clean up bad whitespace
(setq whitespace-style '(trailing space-before-tab indentation empty space-after-tab)) ;; only show bad whitespace
(global-whitespace-mode t)
