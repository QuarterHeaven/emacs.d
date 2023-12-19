(use-package conda
  :straight t
  :defer t
  :config
  (conda-env-initialize-interactive-shells)                        ;; support interactive shell
  (conda-env-initialize-eshell)                                    ;; support eshell
  ;; (conda-env-autoactivate-mode t)                                  ;; auto-activation
  (custom-set-variables '(conda-anaconda-home "~/miniconda3/"))     ;; specify installation directory
  )

(use-package quickrun
  :straight t
  :defer t
  :bind (("C-c r"  . quickrun))
  :config
  (setq quickrun-focus-p nil))

;; [elec-pair]
(use-package elec-pair
  :disabled
  :hook ((prog-mode conf-mode yaml-mode org-mode markdown-mode) . electric-pair-mode)
  :config
  (setq electric-pair-inhibit-predicate 'electric-pair-default-inhibit))

(use-package puni
  :straight t
  :defer t
  :bind
  ("C-c C-<backspace>" . #'puni-force-delete)
  :init
  (puni-global-mode)
  (add-hook 'term-mode-hook #'puni-disable-puni-mode))

(use-package ein
  :straight t)

(use-package protobuf-mode
  :straight t)

(use-package devdocs
  :straight t
  :bind ("C-h D" . devdocs-lookup))

(use-package clangd-inactive-regions
  :straight (:type git :host github :repo "fargiolas/clangd-inactive-regions.el")
  :hook
  (eglot-managed-mode . clangd-inactive-regions)
  :config
  (clangd-inactive-regions-set-method "darken-foreground")
  (clangd-inactive-regions-set-opacity 0.55))

(provide 'init-languages)
