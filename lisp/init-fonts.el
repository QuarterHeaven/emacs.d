;; (require-package 'cnfonts)

;; (setq cnfonts-profiles
;;     '("program" "org-mode" "read-book"))
;; (cnfonts-mode 1)

;; (setq cnfonts-use-face-font-rescale t)

;; (setq cnfonts-personal-fontnames '(("EB Garamond" "Linux Biolinum O")
;; 				   ("LXGW WenKai" "LXGW WenKai Mono")
;; 				   ("Simsun-ExtB")
;; 				   ("Segoe UI Symbol")))

(require 'org)
(require 'cl-lib)
(require 'color)

(centaur-setup-fonts)
(add-hook 'window-setup-hook #'centaur-setup-fonts)
(add-hook 'server-after-make-frame-hook #'centaur-setup-fonts)

(require 'color)
(require 'ef-themes)
(with-eval-after-load 'org-faces
    (set-face-attribute 'variable-pitch nil :family "EB Garamond" :height 160)
    (set-face-attribute 'org-level-1 nil :weight 'semi-bold :family "EB Garamond" :height 240)
    (set-face-attribute 'org-level-2 nil :family "Linux Biolinum O" :height 210)
    (set-face-attribute 'org-level-3 nil :family "Linux Biolinum O" :height 190)
    (set-face-attribute 'org-level-4 nil :family "Linux Biolinum O" :height 160)
    (set-face-attribute 'fixed-pitch nil :family "FiraCode Nerd Font Mono")
    ;; (set-face-attribute 'org-block nil :background
    ;;                     (color-darken-name
    ;;                      (face-attribute 'default :background) 3) :inherit 'fixed-pitch :family "Fira Code")
    (set-face-attribute 'org-block nil :inherit 'fixed-pitch)
    ;; (set-face-attribute 'org-code nil :background
    ;;                     (color-darken-name
    ;;                      (face-attribute 'default :background) 3) :inherit 'fixed-pitch :family "Fira Code")
    ;; (set-face-attribute 'org-quote nil :background
    ;;                     (color-darken-name
    ;;                      (face-attribute 'default :background) 3) :inherit 'fixed-pitch :family "Fira Code")
    ;; (set-face-attribute 'org-block-begin-line nil :background
    ;;                     "#F1E6F8")
    (set-face-attribute 'org-block-begin-line nil :inherit 'fixed-pitch)
    ;; (set-face-attribute 'org-block-end-line nil :background
    ;;                     (color-darken-name
    ;;                      (face-attribute 'default :background) 4) :inherit 'fixed-pitch :family "Fira Code")
    (set-face-attribute 'org-block-begin-line nil :inherit 'fixed-pitch)
)

(setq ef-themes-headings
      (quote ((1 semibold 1.5)
	      (2 1.3)
	      (3 1.2)
	      (t variable-pitch))))

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
	 (:strike-through t)))
)

(provide 'init-fonts)
