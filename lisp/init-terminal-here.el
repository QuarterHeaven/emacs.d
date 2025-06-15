;; -*- lexical-binding: t -*-
(use-package terminal-here
  :straight (:host github :repo "davidshepherd7/terminal-here")
  :config
  (setq terminal-here-linux-terminal-command 'foot))

(provide 'init-terminal-here)
