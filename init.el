;; stask's Emacs 24 configuration
;;
;; started from scratch after stupid 'rm -rf .emacs.d' incident

(setq sk-emacs-init-file (or load-file-name buffer-file-name))
(setq sk-emacs-config-dir
      (file-name-directory sk-emacs-init-file))
(setq user-emacs-directory sk-emacs-config-dir)
(setq sk-init-dir (expand-file-name "init.d" sk-emacs-config-dir))

;; Load all elisp files in ./init.d
(if (file-exists-p sk-init-dir)
    (dolist (file (directory-files sk-init-dir t "\\.el$"))
      (load file)))

;; save customizations in a separate file
(setq custom-file "~/.emacs.d/customizations.el")
(load custom-file)
