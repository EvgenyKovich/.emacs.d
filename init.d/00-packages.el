(add-to-list 'load-path (expand-file-name "el-get/el-get" user-emacs-directory))

;; setup el-get
(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

;; packages
(setq el-get-sources
      '((:name clojure-mode
               :type git
               :url "git://github.com/technomancy/clojure-mode.git")
        (:name midje-mode
               :type git
               :url "git://github.com/dnaumov/midje-mode.git")
        (:name nrepl
               :type git
               :url "git@github.com:clojure-emacs/nrepl.el.git")
        (:name color-theme-solarized
               :type git
               :url "git://github.com/sellout/emacs-color-theme-solarized.git"
               :after (progn
                        (add-to-list 'custom-theme-load-path
                                     (expand-file-name "el-get/color-theme-solarized"
                                                       user-emacs-directory))))
        (:name ac-nrepl
               :type git
               :url "git://github.com/purcell/ac-nrepl.git")
        (:name paredit
               :type git
               :url "git://github.com/emacsmirror/paredit.git")
        (:name rainbow-delimiters
               :type git
               :url "git://github.com/jlr/rainbow-delimiters.git")
        (:name php-mode
               :type git
               :url "git://github.com/ejmr/php-mode.git")
        (:name actionscript-mode
               :type git
               :url "git://github.com/austinhaas/actionscript-mode.git")
        (:name jade-mode
               :type git
               :url "git://github.com/brianc/jade-mode.git")
        (:name coffee-mode
               :type git
               :url "git://github.com/defunkt/coffee-mode.git")
        (:name markdown-mode
               :type git
               :url "git://jblevins.org/git/markdown-mode.git")
        (:name yaml-mode
               :type git
               :url "git://github.com/yoshiki/yaml-mode.git")
        (:name rvm
               :type git
               :url "git://github.com/senny/rvm.el.git")
        (:name magit
               :type git
               :url "git://github.com/magit/magit.git")
        (:name scala-mode2
               :type git
               :url "git://github.com/hvesalai/scala-mode2.git")
        (:name elixir-mode
               :type git
               :url "git://github.com/elixir-lang/emacs-elixir.git")
        (:name multi-term
               :type git
               :url "git://github.com/emacsmirror/multi-term.git")
        (:name dash-at-point
               :type git
               :url "git@github.com:stanaka/dash-at-point.git")
        (:name js2-mode
               :type git
               :url "git@github.com:mooz/js2-mode.git")
        (:name rinari
               :type git
               :url "git@github.com:eschulte/rinari.git")
        (:name rhtml
               :type git
               :url "git@github.com:eschulte/rhtml.git")))

(setq my-packages
      (append
       '(clojure-mode midje-mode nrepl color-theme-solarized ac-nrepl paredit
                      rainbow-delimiters php-mode actionscript-mode jade-mode
                      coffee-mode markdown-mode yaml-mode sml-mode rvm magit
                      scala-mode2 elixir-mode multi-term dash-at-point js2-mode
                      rinari rhtml)
       (mapcar 'el-get-source-name el-get-sources)))

(el-get 'sync my-packages)
