(setq erc-prompt-for-nickserv-password nil)
(add-hook 'erc-after-connect
          '(lambda (SERVER NICK)
             (cond
              ((string-match "freenode\\.net" SERVER)
               (erc-message "PRIVMSG" (concat "NickServ identify " freenode-stask-passwd))))))
