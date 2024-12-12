(use-package direnv
  :straight (direnv :type git :host github :repo "wbolster/emacs-direnv")
  :config
  (direnv-mode))

(provide 'init-direnv)
