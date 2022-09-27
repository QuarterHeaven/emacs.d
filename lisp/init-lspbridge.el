; (straight-use-package
;  '(lsp-bridge :type git :host github :repo "manateelazycat/lsp-bridge"))
(add-to-list 'load-path "~/.emacs.d/straight/repos/lsp-bridge/")

(require-package 'markdown-mode)

(require 'yasnippet)
(yas-global-mode 1)

(require 'lsp-bridge)
(global-lsp-bridge-mode)

(provide 'init-lsp)
