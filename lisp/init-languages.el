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
  :hook ((prog-mode conf-mode yaml-mode org-mode markdown-mode) . electric-pair-mode)
  :config
  (setq electric-pair-inhibit-predicate 'electric-pair-default-inhibit))

(use-package ein
  :straight t)

(provide 'init-languages)
