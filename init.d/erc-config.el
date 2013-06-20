(setq erc-autojoin-channels-alist
      '(("freenode.net" "#emacs" "#clojure" "#redis")))
(defun stask-irc-freenode ()
  (interactive)
  (erc-select :server "irc.freenode.net"
              :port 6667
              :password freenode-stask-passwd
              :nick "stask"))
