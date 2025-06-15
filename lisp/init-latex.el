;; -*- lexical-binding: t -*-
(use-package latex
  :after tex
  :straight auctex
  :hook ((LaTeX-mode . electric-pair-mode)))

(use-package cdlatex
  :after latex
  :straight t
  ;; :commands turn-on-cdlatex
  :hook (LaTeX-mode . turn-on-cdlatex))

(provide 'init-latex)
