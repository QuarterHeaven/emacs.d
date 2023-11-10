(use-package ace-window
  :straight t
  :config
  (global-set-key (kbd "C-x o") 'ace-window))

(use-package auto-dim-other-buffers
  :straight t
  :hook
  (after-init . auto-dim-other-buffers-mode))

;; (eyebrowse-mode t)
;; (eyebrowse-setup-opinionated-keys)

;; (desktop-save-mode 1)
;; (push '(company-posframe-mode . nil)
;;       desktop-minor-mode-table)
;; (run-with-idle-timer 30 t (lambda () (desktop-save "~/.emacs.d")))

(provide 'init-window)
