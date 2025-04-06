;;; init-eee.el --- Configs of https://github.com/eval-exec/eee.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  TakaObsid

;; Author: TakaObsid <takaobsid@Leyline>
;; Keywords: lisp, terminals,

(use-package eee
  :straight '(:type git :host github :repo "eval-exec/eee.el"
                  :files (:defaults "*.el" "*.sh"))
  :config
  (setq ee-terminal-command "wezterm")
  )

(provide 'init-eee)
