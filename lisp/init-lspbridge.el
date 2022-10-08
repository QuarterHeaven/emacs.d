(add-to-list 'load-path "/Users/takaobsid/.emacs.d/lsp-bridge/")

(require-package 'markdown-mode)

(require 'lsp-bridge)
(global-lsp-bridge-mode)

(setq lsp-bridge-org-babel-lang-list '("rust" "c" "cpp" "python" "latex"))
(setq lsp-bridge-enable-auto-format-code t)
(setq acm-enable-tabnine t)
(setq lsp-bridge-enable-log t)

(provide 'init-lspbridge)
