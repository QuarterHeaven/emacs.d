(require 'quickrun)
(require 'conda)
(add-to-list 'load-path "~/.emacs.d/site-lisp/emacs-ipython-notebook/lisp")
(require 'ein)

(setq lsp-bridge-python-command "~/miniconda3/bin/python3.10")
(conda-env-initialize-interactive-shells)                        ;; support interactive shell
(conda-env-initialize-eshell)                                    ;; support eshell
(conda-env-autoactivate-mode t)                                  ;; auto-activation
(custom-set-variables '(conda-anaconda-home "~/miniconda3/"))     ;; specify installation directory
(add-hook 'conda-post-activate-hook                              ;; restart lsp-bridge
          (lambda ()
            (lsp-bridge-restart-process)))

(provide 'init-languages)
