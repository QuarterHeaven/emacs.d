;;; init-topsy.el ---                                -*- lexical-binding: t; -*-

;; Copyright (C) 2025  TakaObsid

;; Author: TakaObsid <takaobsid@Leyline>
;; Keywords: lisp,

(use-package topsy
  :disabled
  :straight (:host github :repo "alphapapa/topsy.el")
  :hook
  (prog-mode . topsy-mode)
  (magit-section-mode . topsy-mode))

(use-package which-function-mode
  :disabled
  :hook
  (prog-mode . which-function-mode)
  :config
  (setq mode-line-format (delete (assoc 'which-func-mode
                                        mode-line-format) mode-line-format)
        which-func-header-line-format '(which-func-mode ("" which-func-format)))
  (defadvice which-func-ff-hook (after header-line activate)
    (when which-func-mode
      (setq mode-line-format (delete (assoc 'which-func-mode
                                            mode-line-format) mode-line-format)
            header-line-format which-func-header-line-format))))

(use-package window-stool
  :disabled
  :straight (:host github :repo "JasZhe/window-stool")
  :config
  (add-hook 'prog-mode-hook #'window-stool-mode))

;;; treesitter-context-mode
(use-package posframe-plus
  :straight (posframe-plus :host github :repo "zbelial/posframe-plus"))

(use-package treesitter-context
  :disabled
  :after (posframe-plus)
  :straight (treesitter-context :host github :repo "zbelial/treesitter-context.el")
  :hook (rust-ts-mode . #'treesitter-context-mode)
  (c++-ts-mode . #'treesitter-context-mode)
  (c-ts-mode . #'treesitter-context-mode)
  (go-ts-mode . #'treesitter-context-mode)
  (java-ts-mode . #'treesitter-context-mode))

(provide 'init-context)
