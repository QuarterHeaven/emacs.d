;; (require-package 'cnfonts)

;; (setq cnfonts-profiles
;;     '("program" "org-mode" "read-book"))
;; (cnfonts-mode 1)

;; (setq cnfonts-use-face-font-rescale t)

;; (setq cnfonts-personal-fontnames '(("EB Garamond" "Linux Biolinum O")
;; 				   ("LXGW WenKai" "LXGW WenKai Mono")
;; 				   ("Simsun-ExtB")
;; 				   ("Segoe UI Symbol")))
(require 'org-faces)
(require-package 'color)

;; fonts set copy from centaur
(defun font-installed-p (font-name)
  "Check if font with FONT-NAME is available."
  (find-font (font-spec :name font-name)))

(defun centaur-setup-fonts ()
  "Setup fonts."
  (when (display-graphic-p)
    ;; Set default font
    (cl-loop for font in '("Fira Code" "Jetbrains Mono"
                           "SF Mono" "Hack" "Source Code Pro" "Menlo"
                           "Monaco" "DejaVu Sans Mono" "Consolas")
             when (font-installed-p font)
             return (set-face-attribute 'default nil
                                        :family font
                                        :height (cond (sys/macp 130)
                                                      (sys/win32p 110)
                                                      (t 100))))

    ;; Set mode-line font
    (cl-loop for font in '("Menlo" "SF Pro Display" "Helvetica")
             when (font-installed-p font)
             return (progn
                      (set-face-attribute 'mode-line nil :family font :height 120)
                      (when (facep 'mode-line-active)
                        (set-face-attribute 'mode-line-active nil :family font :height 120))
                      (set-face-attribute 'mode-line-inactive nil :family font :height 120)))
    
    ;; Specify font for all unicode characters
    (cl-loop for font in '("Segoe UI Symbol" "Symbola" "Symbol")
             when (font-installed-p font)
             return (if (< emacs-major-version 27)
                        (set-fontset-font "fontset-default" 'unicode font nil 'prepend)
                      (set-fontset-font t 'symbol (font-spec :family font) nil 'prepend)))

    ;; Emoji
    (cl-loop for font in '("Noto Color Emoji" "Apple Color Emoji" "Segoe UI Emoji")
             when (font-installed-p font)
             return (cond
                     ((< emacs-major-version 27)
                      (set-fontset-font "fontset-default" 'unicode font nil 'prepend))
                     ((< emacs-major-version 28)
                      (set-fontset-font t 'symbol (font-spec :family font) nil 'prepend))
                     (t
                      (set-fontset-font t 'emoji (font-spec :family font) nil 'prepend))))

    ;; Specify font for Chinese characters
    (cl-loop for font in '("LXGW WenKai Mono" "WenQuanYi Micro Hei" "PingFang SC" "Microsoft Yahei" "STFangsong")
             when (font-installed-p font)
             return (progn
                      (setq face-font-rescale-alist `((,font . 1.0)))
                      (set-fontset-font t '(#x4e00 . #x9fff) (font-spec :family font))))))

(centaur-setup-fonts)
(add-hook 'window-setup-hook #'centaur-setup-fonts)
(add-hook 'server-after-make-frame-hook #'centaur-setup-fonts)

(require 'color)
(with-eval-after-load 'org-faces 
    (set-face-attribute 'variable-pitch nil :family "EB Garamond" :height 160)
    (set-face-attribute 'org-level-1 nil :weight 'semi-bold :family "EB Garamond" :height 240)
    (set-face-attribute 'org-level-2 nil :family "Linux Biolinum O" :height 210)
    (set-face-attribute 'org-level-3 nil :family "Linux Biolinum O" :height 190)
    (set-face-attribute 'org-level-4 nil :family "Linux Biolinum O" :height 160)
    (set-face-attribute 'fixed-pitch nil :family "Fira Code")
    (set-face-attribute 'org-block nil :background
                        (color-darken-name
                         (face-attribute 'default :background) 3) :inherit 'fixed-pitch :family "Fira Code")
    (set-face-attribute 'org-code nil :background
                        (color-darken-name
                         (face-attribute 'default :background) 3) :inherit 'fixed-pitch :family "Fira Code")
    (set-face-attribute 'org-quote nil :background
                        (color-darken-name
                         (face-attribute 'default :background) 3) :inherit 'fixed-pitch :family "Fira Code")
    (set-face-attribute 'org-block-begin-line nil :background
                        "#F1E6F8")
    (set-face-attribute 'org-block-end-line nil :background
                        (color-darken-name
                         (face-attribute 'default :background) 4) :inherit 'fixed-pitch :family "Fira Code")
)

(provide 'init-fonts)
