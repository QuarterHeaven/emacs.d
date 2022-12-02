(require-package 'lsp-mode)
(require-package 'lsp-ui)
(require-package 'lsp-ivy)

(add-hook 'c-mode-hook #'lsp)
(add-hook 'c++-mode-hook #'lsp)
(add-hook 'clojure-mode-hook #'lsp)
(add-hook 'rust-mode-hook #'lsp)
(lsp-ui-mode)
(setq lsp-rust-analyzer-inlay-hints-mode t)
(setq lsp-rust-analyzer-server-display-inlay-hints t)

(provide 'init-lspmode)
