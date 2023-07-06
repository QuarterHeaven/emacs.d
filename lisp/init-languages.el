(require 'quickrun)
(require 'conda)
(setq lsp-bridge-python-command "~/miniconda/bin/python3.10")
(conda-env-initialize-interactive-shells)                        ;; support interactive shell
(conda-env-initialize-eshell)                                    ;; support eshell
(conda-env-autoactivate-mode t)                                  ;; auto-activation
(custom-set-variables '(conda-anaconda-home "~/miniconda/"))     ;; specify installation directory
(add-hook 'conda-post-activate-hook                              ;; restart lsp-bridge
          (lambda ()
            (lsp-bridge-restart-process)))
(provide 'init-languages)
