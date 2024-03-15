(use-package lsp-mode
  :straight t
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
	 ((c-mode c-ts-mode) . lsp)
         ((c++-mode c++-ts-mode) . lsp)
	 ((python-mode python-ts-mode) . lsp)
	 ((rust-mode rust-ts-mode) . lsp)
	 ((clojure-mode clojure-ts-mode) . lsp)
	 ;; if you want which-key integration
         ;; (lsp-mode . lsp-enable-which-key-integration)
	 )
  :commands lsp
  :config
  (setq lsp-rust-analyzer-inlay-hints-mode t)
  (setq lsp-rust-analyzer-server-display-inlay-hints t))

;; optionally
(use-package lsp-ui :commands lsp-ui-mode)
(use-package lsp-treemacs :commands lsp-treemacs-errors-list)

;; optionally if you want to use debugger
;; (use-package dap-mode)
;; (use-package dap-LANGUAGE) to load the dap adapter for your language

;; optional if you want which-key integration
;; (use-package which-key
;;     :config
;;     (which-key-mode))


(provide 'init-lspmode)
