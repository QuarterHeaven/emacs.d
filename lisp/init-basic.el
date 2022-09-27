(setq make-backup-files nil)
(setq-default auto-save-timeout 15) ; 15秒无动作,自动保存
(setq-default auto-save-interval 100) ; 100个字符间隔, 自动保存

(setq url-gateway-method 'socks)
(setq socks-server '("Default server" "127.0.0.1" 1081 5))

(setq url-gateway-local-host-regexp
      (concat "\\`" (regexp-opt '("localhost" "127.0.0.1")) "\\'"))

(setq truncate-lines t)

; 当前窗口打开 dired
(put 'dired-find-alternate-file 'disabled nil)

; 默认窗口最大化
(setq initial-frame-alist (quote ((fullscreen . maximized))))

(provide 'init-basic)
