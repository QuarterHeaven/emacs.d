;;; init-float-narrow-indirect.el ---                -*- lexical-binding: t; -*-

;; Copyright (C) 2025  TakaObsid

;; Author: TakaObsid <takaobsid@Leyline>
;; Keywords: lisp,

(use-package float-narrow-indirect
  :straight (:host github :repo "yibie/float-narrow-indirect")
  :bind (("C-c n f" . 'fni-narrow-to-region-floating)
         ("C-c n t" . 'fni-toggle-focus)
         ("C-c n c" . 'fni-clear-aggregation)))

