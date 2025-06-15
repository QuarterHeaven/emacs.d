;; -*- lexical-binding: t -*-
(use-package auto-complete
  :straight t)

(use-package ejc-sql
  :straight t
  :after auto-complete
  :hook
  (ejc-sql-minor-mode . (lambda ()
			  (auto-complete-mode t)
			  (ejc-ac-setup)))
  :config
  (setq clomacs-httpd-default-port 8090))

(provide 'init-ejc-sql)
