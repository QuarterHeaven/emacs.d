;;; init-bufferlo.el ---                             -*- lexical-binding: t; -*-

;; Copyright (C) 2025  TakaObsid

;; Author: TakaObsid <takaobsid@Leyline>
;; Keywords: lisp,

(use-package bufferlo
  :straight t
  :init
  (bufferlo-mode)
  (bufferlo-anywhere-mode)
  (setq bufferlo-menu-bar-show t)
  (setq bufferlo-menu-bar-list-buffers 'ibuffer)
  :config
  (defvar my:bufferlo-consult--source-local-buffers
    (list :name "Bufferlo Local Buffers"
          :narrow   ?l
          :category 'buffer
          :face     'consult-buffer
          :history  'buffer-name-history
          :state    #'consult--buffer-state
          :default  t
          :items    (lambda () (consult--buffer-query
                                :predicate #'bufferlo-local-buffer-p
                                :sort 'visibility
                                :as #'buffer-name)))
    "Local Bufferlo buffer candidate source for `consult-buffer'.")

  (defvar my:bufferlo-consult--source-other-buffers
    (list :name "Bufferlo Other Buffers"
          :narrow   ?b
          :category 'buffer
          :face     'consult-buffer
          :history  'buffer-name-history
          :state    #'consult--buffer-state
          :items    (lambda () (consult--buffer-query
                                :predicate #'bufferlo-non-local-buffer-p
                                :sort 'visibility
                                :as #'buffer-name)))
    "Non-local Bufferlo buffer candidate source for `consult-buffer'.")

  ;; add in the reverse order of display preference
  (add-to-list 'consult-buffer-sources 'my:bufferlo-consult--source-other-buffers)
  (add-to-list 'consult-buffer-sources 'my:bufferlo-consult--source-local-buffers))

(provide 'init-bufferlo)
