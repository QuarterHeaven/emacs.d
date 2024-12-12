(use-package projectile
  :straight t
  :bind ("M-p p" . projectile-switch-project)
  ("M-p f" . projectile-find-file)
  :init
  (projectile-mode +1)
  :config
  (setq projectile-enable-caching t)
  (setq projectile-require-project-root t)
  ;; Recommended keymap prefix on macOS
  (define-key projectile-mode-map (kbd "M-p") 'projectile-command-map))

(provide 'init-projectile)
