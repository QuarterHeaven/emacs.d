;; -*- lexical-binding: t -*-
(use-package keyfreq
  :straight t
  :init
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1)

  :config
  (setq keyfreq-excluded-commands
	'(self-insert-command
          forward-char
          backward-char
          previous-line
          next-line)))

(provide 'init-keyfreq)
