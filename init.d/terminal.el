;; fix the PATH variable (http://www.mail-archive.com/clojure@googlegroups.com/msg36929.html)
(defun set-exec-path-from-shell-PATH ()
  (let ((path-from-shell (shell-command-to-string "TERM=dumb $SHELL -i -c 'echo $PATH'")))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

(when window-system (set-exec-path-from-shell-PATH))

(setq multi-term-program "/bin/zsh")
(setq term-term-name "xterm-256color")
(set-terminal-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(add-hook 'term-mode-hook
          (lambda ()
            (define-key term-raw-map (kbd "C-y") 'term-paste)
            (define-key term-raw-map (kbd "s-v") 'term-paste)
            (define-key term-raw-map (kbd "M-<left>") 'term-send-backward-word)
            (define-key term-raw-map (kbd "M-<right>") 'term-send-forward-word)
            (global-unset-key (kbd "C-r"))))


;; Eshell aliases
(setq eshell-aliases-file "~/.emacs.d/eshell-aliases")
