(use-package yasnippet
  :defer t
  :straight t
  :init
  (yas-global-mode 1))

;(use-package auto-yasnippet
;  :bind
;  (("C-c & w" . aya-create)
;   ("C-c & y" . aya-expand))
;  :config
;  (setq aya-persist-snippets-dir (concat user-emacs-directory "snippets")))

(provide 'init-yasnippet)
