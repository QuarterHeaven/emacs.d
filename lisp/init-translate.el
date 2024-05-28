(use-package emacs-immersive-translate
  :straight (:type git :host github :repo "Elilif/emacs-immersive-translate")
  :hook
  (elfeed-show-mode . #'immersive-translate-setup)
  (nov-pre-html-render . #'immersive-translate-setup)

  :init
  ;; use ChatGPT
  (setq immersive-translate-backend 'chatgpt
	immersive-translate-chatgpt-host "api.openai.com"))

(provide 'init-translate)
