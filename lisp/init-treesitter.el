(require-package 'tree-sitter)
(require-package 'tree-sitter-langs)
;; (global-tree-sitter-mode)

;; (add-hook 'rust-mode-hook #'tree-sitter-mode)
;; (add-hook 'c-mode-hook #'tree-sitter-mode)
;; (add-hook 'c++-mode-hook #'tree-sitter-mode)
;; (add-hook 'python-mode-hook #'tree-sitter-mode)
;; (add-hook 'yaml-mode-hook #'tree-sitter-mode)
;; (add-hook 'java-mode-hook #'tree-sitter-mode)

(add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)
;; (add-hook 'prog-mode-hook #'tree-sitter-hl-mode)

(tree-sitter-load 'elisp "elisp")
(add-to-list 'tree-sitter-major-mode-language-alist '(emacs-lisp-mode . elisp))

(provide 'init-treesitter)
