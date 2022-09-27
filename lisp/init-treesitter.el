(require-package 'tree-sitter)
(require-package 'tree-sitter-langs)
(global-tree-sitter-mode)
(add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)

(provide 'init-treesitter)
