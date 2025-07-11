;; -*- lexical-binding: t -*-
(require 'cl-lib)
(require 'color)

(centaur-setup-fonts)
(add-hook 'window-setup-hook #'centaur-setup-fonts)
(when (daemonp)
    (add-hook 'server-after-make-frame-hook #'centaur-setup-fonts))

(setq buffer-face-mode-face 'fixed-pitch)

(defface my-org-emphasis-bold
  '((default :inherit bold)
    (((class color) (min-colors 88) (background light))
     :foreground "#a60000")
    (((class color) (min-colors 88) (background dark))
     :foreground "#ff8059"))
  "My bold emphasis for Org.")

(setq org-emphasis-alist
      '(("*" my-org-emphasis-bold)
	("/" italic)
	("_" underline)
	("=" org-verbatim verbatim)
	("~" org-code verbatim)
	("+"
	 (:strike-through t))))

(use-package unicad
  ;; auto detect file coding-system
  :straight t
  :init
  (setq unicad-mode 1))

(use-package nerd-icons
  :config
  (setq nerd-icons-font-family "TriplicateT4c Nerd Font Propo"))

(provide 'init-fonts)
