(require-package 'markdown-mode)

;; (require 'lsp-bridge)
;; (global-lsp-bridge-mode)

;; (setq lsp-bridge-org-babel-lang-list '("rust" "c" "cpp" "python" "latex"))
;; (setq lsp-bridge-enable-auto-format-code t)
;; (setq acm-enable-tabnine nil)
;; (setq lsp-bridge-enable-log t)
(use-package lsp-bridge
  :config
  (global-lsp-bridge-mode)
  (setq lsp-bridge-org-babel-lang-list '("rust" "c" "cpp" "python" "latex"))
  (setq lsp-bridge-enable-auto-format-code t)
  (setq acm-enable-tabnine nil)
  (setq lsp-bridge-enable-log t))

(provide 'init-lspbridge)
