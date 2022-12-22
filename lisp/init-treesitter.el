(when (treesit-available-p)
  (push '(c++-mode . c++-ts-mode) major-mode-remap-alist)
  (push '(c-mode . c-ts-mode) major-mode-remap-alist)
  (push '(rust-mode . rust-ts-mode) major-mode-remap-alist)
  (push '(go-mode . go-ts-mode) major-mode-remap-alist))

(provide 'init-treesitter)
