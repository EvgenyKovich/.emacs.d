;; clojure
;;(defun turn-on-paredit () (paredit-mode 1))
;;(add-hook 'clojure-mode-hook 'turn-on-paredit)

;; rainbow-delimiters in all programming modes
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;; php
(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))

;; ActionScript
(add-to-list 'auto-mode-alist '("\\.as$" . actionscript-mode))
