;;; init-image.el --- Image display settings         -*- lexical-binding: t; -*-

;; Copyright (C) 2024  TakaObsid

;; Author: TakaObsid <takaobsid@Leyline>
;; Keywords: lisp, multimedia,
(use-package image-slicing
  :straight (:host github :repo "ginqi7/image-slicing")
  :hook (after-init . image-slicing-mode)
  )

(provide 'init-image)
