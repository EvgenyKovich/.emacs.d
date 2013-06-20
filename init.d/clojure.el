;; hide *nrepl-connection* and *nrepl-server* buffers
(setq nrepl-hide-special-buffers t)
(setq nrepl-popup-stacktraces t)
(setq nrepl-history-file "~/.emacs.d/nrepl-history")

(add-hook 'nrepl-connected-hook
          (defun pnh-clojure-mode-eldoc-hook ()
            (add-hook 'clojure-mode-hook 'turn-on-eldoc-mode)
            (add-hook 'nrepl-interaction-mode-hook 'nrepl-turn-on-eldoc-mode)
            (nrepl-enable-on-existing-clojure-buffers)))

;; enable CamelCase for editing commands (for Java)
(add-hook 'nrepl-mode-hook 'subword-mode)

;; enable paredit in nrepl
(add-hook 'nrepl-mode-hook 'paredit-mode)

;; enable rainbow delimiters in nrepl
(add-hook 'nrepl-mode-hook 'rainbow-delimiters-mode)

;(add-hook 'nrepl-mode-hook 'ac-nrepl-setup)
;(add-hook 'nrepl-interaction-mode-hook 'ac-nrepl-setup)
;(eval-after-load "auto-complete"
;  '(add-to-list 'ac-modes 'nrepl-mode))

;; add paredit to clojure-mode
(add-hook 'clojure-mode-hook 'paredit-mode)

;; Teach compile the syntax of the kibit output
(require 'compile)
(add-to-list 'compilation-error-regexp-alist-alist
             '(kibit "At \\([^:]+\\):\\([[:digit:]]+\\):" 1 2 nil 0))
(add-to-list 'compilation-error-regexp-alist 'kibit)

;; A convenient command to run "lein kibit" in the project to which
;; the current emacs buffer belongs to.
(defun kibit ()
  "Run kibit on the current project.
Display the results in a hyperlinked *compilation* buffer."
  (interactive)
  (compile "lein kibit"))

;; add midje
(add-hook 'clojure-mode-hook 'midje-mode)
