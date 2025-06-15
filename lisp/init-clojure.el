;; -*- lexical-binding: t -*-
(use-package clojure-mode
  :straight t
  :defer t
  :config
  (setq clojure-toplevel-inside-comment-form t))

(use-package inf-clojure
  :straight t
  :after (clojure-mode))

(use-package cider
  :straight t
  :after (clojure-mode))

(use-package clj-refactor
  :straight t
  :after (clojure-mode yasnippet)
  :hook (clojure-mode . clj-refactor-mode))

(provide 'init-clojure)
