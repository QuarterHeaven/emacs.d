;;; outli
(use-package outli
  :straight (:host github :repo "jdtsmith/outli")
  ;; :after lispy ; only if you use lispy; it also sets speed keys on headers!
  :bind (:map outli-mode-map ; convenience key to get back to containing heading
	      ("C-c C-p" . (lambda () (interactive) (outline-previous-visible-heading)))
	      ("C-c C-n" . (lambda () (interactive) (outline-next-visible-heading@fix-for-org-fold)))
	      ("C-c C-u" . (lambda () (interactive) (outline-up-heading))))
  :hook ((prog-mode text-mode) . outli-mode)) ; or whichever modes you prefer

(provide 'init-outli)
