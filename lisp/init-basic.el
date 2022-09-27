(setq url-gateway-method 'socks)
(setq socks-server '("Default server" "127.0.0.1" 1081 5))

(setq url-gateway-local-host-regexp
      (concat "\\`" (regexp-opt '("localhost" "127.0.0.1")) "\\'"))

(provide 'init-basic)
