(use-package pixel-scroll
  :disabled t
  :config
  ;; (setq make-it-local t)
  ;; (add-hook 'org-mode-hook 'pixel-scroll-precision-mode-hook nil 'make-it-local)
  ;; (add-hook 'org-mode-hook 'pixel-scroll-precision-mode-hook nil t)
  (pixel-scroll-precision-mode 1)
  (add-hook 'telega-root-mode 'pixel-scroll-precision-mode-hook nil t)
  (add-hook 'telega-chat-mode 'pixel-scroll-precision-mode-hook nil t)
  (setq pixel-scroll-precision-interpolate-page t)
  (defun +pixel-scroll-interpolate-down (&optional lines)
    (interactive)
    (if lines
	(pixel-scroll-precision-interpolate (* -1 lines (pixel-line-height)))
      (pixel-scroll-interpolate-down)))

  (defun +pixel-scroll-interpolate-up (&optional lines)
    (interactive)
    (if lines
	(pixel-scroll-precision-interpolate (* lines (pixel-line-height))))
    (pixel-scroll-interpolate-up))

  (defalias 'scroll-up-command '+pixel-scroll-interpolate-down)
  (defalias 'scroll-down-command '+pixel-scroll-interpolate-up)
  )

(use-package good-scroll
  :straight (:host github :repo "io12/good-scroll.el")
  :init
  (good-scroll-mode 1)
  :config
  (global-set-key [next] #'good-scroll-up-full-screen)
  (global-set-key [prior] #'good-scroll-down-full-screen))

(use-package ultra-scroll
					;:load-path "~/code/emacs/ultra-scroll" ; if you git clone'd instead of package-vc-install
  :straight (:host github :repo "jdtsmith/ultra-scroll")
  :init
  (setq scroll-conservatively 101 ; important!
        scroll-margin 0)
  (ultra-scroll-mode)
  )

(provide 'init-pixel-scroll)
