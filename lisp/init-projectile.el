(use-package projectile
  :straight t
  :bind ("M-p p" . projectile-switch-project)
  ("M-p f" . projectile-find-file)
  :config
  (projectile-mode +1)
  (setq projectile-enable-caching t)
  ;; Recommended keymap prefix on macOS
  (define-key projectile-mode-map (kbd "M-p") 'projectile-command-map))

(provide 'init-projectile)
