(use-package blink-search
  :straight (blink-search :type git :repo "manateelazycat/blink-search")
  :bind
  ("M-s M-s" . blink-search)
  :config
  (setq blink-search-browser-function 'xwidget-webkit-browse-url))

(provide 'init-blink)
