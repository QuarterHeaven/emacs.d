;;; init-yazi.el ---                                 -*- lexical-binding: t; -*-

;; Copyright (C) 2026  TakaObsid

;; Author: TakaObsid <takaobsid@Leyline>
;; Keywords:

(use-package yazi.el
  :straight (:host github :repo "bommbo/yazi.el")
  :config
  (add-hook 'term-mode-hook
         (lambda ()
           (meow-mode -1)
           (setq-local meow-normal-mode nil))))

(provide 'init-yazi)
