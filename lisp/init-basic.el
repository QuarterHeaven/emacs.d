(setq make-backup-files nil)
(setq-default auto-save-timeout 15) ; 15秒无动作,自动保存
(setq-default auto-save-interval 100) ; 100个字符间隔, 自动保存

(setq url-gateway-method 'socks)
(setq socks-server '("Default server" "127.0.0.1" 1081 5))

(setq url-gateway-local-host-regexp
      (concat "\\`" (regexp-opt '("localhost" "127.0.0.1")) "\\'"))

(provide 'init-basic)
