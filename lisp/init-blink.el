;; -*- lexical-binding: t -*-
(use-package blink-search
  :straight (blink-search :type git :repo "manateelazycat/blink-search" :files (:defaults "backend/*.el"))
  :bind
  ("M-s M-s" . blink-search)
  :config
  (setq blink-search-browser-function 'xwidget-webkit-browse-url))

(provide 'init-blink)
