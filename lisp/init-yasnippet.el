(require-package 'yasnippet)
(yas-global-mode 1)

(require-package 'yasnippet-snippets)

(require-package 'auto-yasnippet)

(use-package auto-yasnippet
  :bind
  (("C-c & w" . aya-create)
   ("C-c & y" . aya-expand))
  :config
  (setq aya-persist-snippets-dir (concat user-emacs-directory "snippets")))

(provide 'init-yasnippet)

