;;;init-themes.el---Defaultsforthemes-*-lexical-binding:t-*-
;;;Commentary:
;;;Code:

;;; common theme settings
(use-package all-the-icons
  :straight t)

(setq inhibit-compacting-font-caches t)

;;Don't prompt to confirm theme safety. This avoids problems with
;;first-time startup on Emacs > 26.3.
(setq custom-safe-themes t)

;; (use-package highlight-indent-guides
;;   :straight t
;;   :hook
;;   (prog-mode . highlight-indent-guides-mode)
;;   :config
;;   (setq highlight-indent-guides-method 'fill))
(use-package indent-bars
  :straight (indent-bars :type git :host github :repo "jdtsmith/indent-bars")
  :hook (prog-mode . indent-bars-mode)
  ;; :config
  ;; (custom-set-faces
  ;;     '(indent-bars-face ((t (:height 120)))))
  :custom
  (indent-bars-treesit-support t)
  (indent-bars-no-descend-string t)
  (indent-bars-width-frac 0.1)
  (indent-bars-pad-frac 0.1)
  (indent-bars-color '(highlight :face-bg t :blend 0.2))
  (indent-bars-color-by-depth '(:regexp "outline-\\([0-9]+\\)" :blend 1))
  (indent-bars-highlight-current-depth '(:blend 0.5))
  (indent-bars-zigzag nil)
  ;; (indent-bars-highlight-current-depth nil) 
  ;; (indent-bars-pattern "|")
  ;; (indent-bars-no-stipple-char ?\⎸)
  (indent-bars-prefer-character nil)
  (indent-bars-display-on-blank-lines nil))

;;; catppuccin
(use-package catppuccin-theme
  :disabled t
  :straight t
  :init
  (load-theme 'catppuccin :no-confirm)
  (setq catppuccin-flavor 'macchiato)
  (catppuccin-reload)
  :config
  (custom-set-faces
   '(font-lock-comment-face ((t (:slant italic :family "Victor Mono" :height 140))))
   '(org-indent ((t (:inherit (org-hide fixed-pitch)))))
   '(org-block ((t (:inherit fixed-pitch))))
   '(org-verbatim ((t (:inherit (shadow fixed-pitch)))))))

;;; doom themes
;;If you don't customize it, this is the theme you get.
;; (setq-default custom-enabled-themes '(doom-city-light))
(use-package autothemer :straight t)

(use-package rose-pine-emacs
  :straight (:host github
                   :repo "thongpv87/rose-pine-emacs"
                   :branch "master")
  :after autothemer)

(use-package doom-themes
  ;; :disabled t
  :straight t
  :init
  ;; (setq current-load-theme 'doom-nord-light)
  ;; (setq current-load-theme 'doom-bluloco-light)
  ;; (setq current-load-theme 'doom-one)
  ;; (setq current-load-theme 'doom-ayu-light)
  ;; (setq current-load-theme 'doom-tomorrow-day)
  ;; (setq current-load-theme 'rose-pine-dawn)
  ;; (setq current-load-theme 'doom-gruvbox)
  ;; (setq current-load-theme 'doom-ayu-mirage)
  (setq current-load-theme 'modus-operandi-tinted)
  (load-theme current-load-theme t)
  ;; :hook
  ;; (highlight-indent-guides-mode . (lambda () (load-theme current-load-theme t)))
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled

  ;; fix cycle inheritance in emacs 31
  (setcdr (assoc 'gnus-group-news-low-empty doom-themes-base-faces)
          '(:inherit 'gnus-group-mail-1-empty :weight 'normal))

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; (doom-themes-neotree-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  ;; (doom-themes-treemacs-config)
 ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config)

  (custom-set-faces
   '(font-lock-comment-face ((t (:slant italic :family "Victor Mono" :height 140))))
   '(org-indent ((t (:inherit (org-hide fixed-pitch)))))
   '(org-block ((t (:inherit fixed-pitch))))
   '(org-verbatim ((t (:inherit (shadow fixed-pitch))))))

  (defun my-load-doom-theme (frame)
    (select-frame frame)
    (load-theme current-load-theme t)
    )

  (if (daemonp)
      (add-hook 'after-make-frame-functions #'my-load-doom-theme)
    (load-theme current-load-theme t)
    ))

;; Ensure that themes will be applied even if they have not been customized
(defun reapply-themes ()
  "Forcibly load the themes listed in `custom-enabled-themes'."
  (dolist (theme custom-enabled-themes)
    (unless (custom-theme-p theme)
      (load-theme theme)))
  (custom-set-variables `(custom-enabled-themes (quote ,custom-enabled-themes))))

;; Toggle between light and dark

;;; sanityinc tomorrow theme
(use-package color-theme-sanityinc-tomorrow
  :disabled t
  :straight t
  :init
  (load-theme 'sanityinc-tomorrow-night t))

;;; ayu theme
(use-package ayu-theme
  :disabled
  :straight (:host github :repo "vutran1710/Ayu-Theme-Emacs")
  :init (load-theme 'ayu-grey t))

;;; icons
(defconst sys/win32p
  (eq system-type 'windows-nt)
  "Are we running on a WinTel system?")

;;; Show line numbers
(use-package display-line-numbers
  :disabled
  :straight t
  :hook ((prog-mode yaml-mode conf-mode) . display-line-numbers-mode)
  :init (custom-set-variables '(display-line-numbers-width-start t)
			      '(display-line-numbers-grow-only t)
			      '(display-line-numbers-width 5)))

;;; Suppress GUI features
(setq use-file-dialog nil
       use-dialog-box nil
       inhibit-startup-screen t
       inhibit-startup-echo-area-message user-login-name
       inhibit-default-init t
       initial-scratch-message nil)
(unless (daemonp)
  (advice-add #'display-startup-echo-area-message :override #'ignore))

;; Display dividers between windows
(setq window-divider-default-places t
      window-divider-default-bottom-width 1
      window-divider-default-right-width 1)
(add-hook 'window-setup-hook #'window-divider-mode)

;;; Smooth scrolling over images
(use-package iscroll
  :straight t
  :diminish
  :hook (image-mode . iscroll-mode))

;;; Use fixed pitch where it's sensible
(use-package mixed-pitch
  :straight t
  :diminish)

;;; Display ugly ^L page breaks as tidy horizontal lines
(use-package page-break-lines
  :straight t
  :diminish
  :hook (after-init . global-page-break-lines-mode))

;;; Don't use GTK+ tooltip
(when (boundp 'x-gtk-use-system-tooltips)
  (setq x-gtk-use-system-tooltips nil))

;;; Mode line
(use-package doom-modeline
  :disabled
  :straight t
  :hook (after-init . doom-modeline-mode)
  :init
  (doom-modeline-mode)
  :config
  (setq doom-modeline-support-imenu t
	doom-modeline-icon t
        doom-modeline-height 1
	doom-modeline-bar-width 4
        doom-modeline-window-width-limit 110
        doom-modeline-minor-modes t
	doom-modeline-project-detection 'auto
	doom-modeline-workspace-name t
	;; doom-modeline-position-column-line-format '("%l:%c")
	doom-modeline-total-line-number t
	doom-modeline-env-version t
	find-file-visit-truename t
	doom-modeline-github t
        doom-modeline-gnus nil
	doom-modeline-github-interval (* 30 60))
  ;; (set-face-attribute 'doom-modeline nil :inherit 'mode-line)
  (doom-modeline-def-segment image-info
    "Display the current image's info on the modeline"
    (concat
     doom-modeline-spc
     (when (eq major-mode 'image-mode)
       ;; Needs imagemagick installed.
       (process-lines "identify" "-format" "[%m %wx%h %b]" (buffer-file-name))))))

;;; Moody - tabs and ribbons for mode-line
(use-package moody
  :straight t
  :demand t
  :config
  (setq x-underline-at-descent-line t
        find-file-visit-truename t)

  ;; 移除主题的 :box 属性，避免与 moody 冲突；缩小字体
  (defun my/face-attr (face attr)
    "获取 FACE 的 ATTR，如果是 unspecified 则返回 nil。"
    (let ((val (face-attribute face attr nil t)))
      (unless (eq val 'unspecified) val)))

  (defun my/color-low-contrast-p (c1 c2)
    "判断两个颜色对比度是否过低。"
    (and (stringp c1) (stringp c2)
         (let* ((rgb1 (color-name-to-rgb c1))
                (rgb2 (color-name-to-rgb c2))
                (lum1 (+ (* 0.299 (nth 0 rgb1)) (* 0.587 (nth 1 rgb1)) (* 0.114 (nth 2 rgb1))))
                (lum2 (+ (* 0.299 (nth 0 rgb2)) (* 0.587 (nth 1 rgb2)) (* 0.114 (nth 2 rgb2)))))
           (< (abs (- lum1 lum2)) 0.1))))

  (defun my/moody-setup-faces ()
    "设置 moody 所需的 face 属性，在主题加载后调用。"
    (let* ((bg (my/face-attr 'mode-line :background))
           (line (my/face-attr 'mode-line :underline)))
      ;; underline 可能是 plist，提取 :color
      (when (listp line)
        (setq line (plist-get line :color)))
      ;; fallback: box color -> border foreground -> mode-line foreground
      (unless (stringp line)
        (let ((box (my/face-attr 'mode-line :box)))
          (setq line (or (and (listp box) (plist-get box :color))
                         (my/face-attr 'vertical-border :foreground)
                         (my/face-attr 'mode-line :foreground)
                         "grey50"))))
      ;; 如果颜色和背景对比度不足，用 foreground 兜底
      (when (my/color-low-contrast-p line bg)
        (setq line (or (my/face-attr 'mode-line :foreground) "grey50")))
      (set-face-attribute 'mode-line          nil
                          :overline nil
                          :underline `(:color ,line :position t)
                          :box nil)
      (set-face-attribute 'mode-line-inactive nil
                          :overline nil
                          :underline `(:color ,line :position t)
                          :box nil)))
  (defun my/moody-setup ()
    "设置 moody faces 并重新应用 moody 替换，切换主题后调用。"
    (my/moody-setup-faces)
    ;; 重新设置 mode-line-format（主题可能重置了它）
    ;; (moody-replace-mode-line-front-space)
    (moody-replace-mode-line-buffer-identification)
    (moody-replace-vc-mode))

  ;; Emacs 29+ 切换主题后自动重新设置
  (add-hook 'enable-theme-functions (lambda (_theme) (my/moody-setup)))

  (defun my/mode-line-project-name ()
    "显示当前项目名称。"
    (when-let* ((project (project-current))
                (name (project-name project)))
      (format " [%s]" name)))

  (defun my/mode-line-total-lines ()
    "显示总行数。"
    (format "/%d" (line-number-at-pos (point-max))))

  (defun my/mode-line-image-info ()
    "在 image-mode 下显示图片信息。"
    (when (eq major-mode 'image-mode)
      (ignore-errors
        (car (process-lines "identify" "-format" " [%m %wx%h %b]" (buffer-file-name))))))

  (defun my/mode-line-encoding ()
    "显示文件编码（仅非 UTF-8 时）。"
    (when buffer-file-coding-system
      (let ((name (symbol-name buffer-file-coding-system)))
        (unless (string-match-p "utf-8" name)
          (format " %s" (upcase name))))))

  ;; GitHub 通知
  (defvar my/github-notification-count 0)

  (defun my/github-fetch-notifications ()
    "异步获取 GitHub 未读通知数量。"
    (when (require 'ghub nil t)
      (ignore-errors
        (ghub-get "/notifications"
                  '((per_page . 100))
                  :callback (lambda (value _headers _status _req)
                              (setq my/github-notification-count (length value))
                              (force-mode-line-update t))
                  :errorback (lambda (&rest _)
                               (setq my/github-notification-count 0))))))

  (defun my/mode-line-github ()
    "显示 GitHub 未读通知数。"
    (when (> my/github-notification-count 0)
      (format " GH:%s"
              (if (> my/github-notification-count 99) "99+"
                (number-to-string my/github-notification-count)))))

  (run-with-idle-timer 30 nil #'my/github-fetch-notifications)
  (run-with-timer (* 30 60) (* 30 60) #'my/github-fetch-notifications)

  ;; 先设置 mode-line-format，再调用 moody 替换函数
  (setq-default mode-line-format
                '("%e"
                  mode-line-front-space
                  mode-line-mule-info
                  mode-line-client
                  mode-line-modified
                  mode-line-remote
                  " "
                  mode-line-buffer-identification
                  "  "
                  mode-line-position
                  (:eval (my/mode-line-total-lines))
                  "  "
                  (vc-mode vc-mode)
                  (:eval (my/mode-line-project-name))
                  (:eval (my/mode-line-image-info))
                  "  "
                  mode-line-modes
                  (:eval (my/mode-line-encoding))
                  (:eval (my/mode-line-github))
                  mode-line-misc-info
                  mode-line-end-spaces))

  ;; 必须在 setq-default mode-line-format 之后调用
  (my/moody-setup)
  (moody-replace-eldoc-minibuffer-message-function))

(use-package hide-mode-line
  :straight t
  :hook (((
	   ;; completion-list-mode
           ;; completion-in-region-mode
           eshell-mode
           shell-mode
           term-mode
           vterm-mode
           pdf-annot-list-mode
           flycheck-error-list-mode) . hide-mode-line-mode)))

(use-package minions
  :straight t
  :hook (after-init . minions-mode))

;;; punch line
;; 替换 doom-modeline
(use-package punch-line
  :disabled
  :straight (:host github :repo "konrad1977/punch-line")
  :hook
  (after-init . punch-line-mode)
  :custom
  (punch-weather-update) ;; Use weather service
  :config
  (setq
   punch-line-separator "  "
   punch-show-project-info t					;; Show project info
   punch-show-git-info t						;; Show git info
   punch-show-lsp-info t						;; Show eglot info
   punch-show-copilot-info t					;; Show copilot
   punch-show-battery-info t					;; Show battery status
   punch-show-weather-info t					;; Weather info
   punch-weather-latitude "30.659462"				;; Weather latitude
   punch-weather-longitude "104.065735"			;; Weather longitude
   punch-line-music-info '(:service apple))		;; Music service
  (punch-line-mode 1))

(use-package flycheck
  :straight t
  :after punch-line)


;;; tab bar
(use-package svg-lib
  :straight t
  )

(use-package svg-tag-mode
  :straight t
  )

(use-package tab-bar
  :hook
  (telega-connection-state-hook . +tab-bar-telega-icon-update)
  (telega-kill-hook . +tab-bar-telega-icon-update)

  :bind
  ("C-c n n" . tab-bar-switch-to-next-tab)
  ("C-c n p" . tab-bar-switch-to-prev-tab)
  ("C-c n t" . tab-bar-new-tab)
  ("C-c n w" . tab-bar-close-tab)
  ("C-c n 1" . (lambda() (interactive) (tab-bar-select-tab 1)))
  ("C-c n 2" . (lambda() (interactive) (tab-bar-select-tab 2)))
  ("C-c n 3" . (lambda() (interactive) (tab-bar-select-tab 3)))
  ("C-c n 4" . (lambda() (interactive) (tab-bar-select-tab 4)))
  ("C-c n 5" . (lambda() (interactive) (tab-bar-select-tab 5)))
  ("C-c n 6" . (lambda() (interactive) (tab-bar-select-tab 6)))
  ("C-c n 7" . (lambda() (interactive) (tab-bar-select-tab 7)))
  ("C-c n 8" . (lambda() (interactive) (tab-bar-select-tab 8)))
  ("C-c n 9" . (lambda() (interactive) (tab-bar-select-tab 9)))
  ("C-c n 0" . (lambda() (interactive) (tab-bar-select-tab 10)))

  :config
  (require 'svg-lib)
  (require 'svg-tag-mode)
  (setq tab-bar-separator ""
        tab-bar-new-tab-choice "*welcome*"
        tab-bar-tab-name-truncated-max 20
        tab-bar-auto-width nil
        tab-bar-close-button-show nil
        tab-bar-tab-hints t
	tab-bar-mode t
	tab-bar-new-tab-to 'rightmost)
  (setq tab-bar-select-tab-modifiers '(hyper))

  ;; [telega]
  (defvar +tab-bar-telega-indicator-cache nil)
  ;; (defun +tab-bar-telega-icon-update (&rest rest)
  ;;   (setq +tab-bar-telega-indicator-cache
  ;;         (when (and (fboundp 'telega-server-live-p)
  ;;                    (telega-server-live-p)
  ;;                    (buffer-live-p telega-server--buffer))
  ;;           (let* ((me-user (telega-user-me 'locally))
  ;;                  (online-p (and me-user (telega-user-online-p me-user)))
  ;;                  (unread-count (and (boundp 'telega--unread-chat-count)
  ;;                                     (plist-get telega--unread-chat-count :unread_unmuted_count))))
  ;;             (propertize (concat " "
  ;;                                 (if online-p "▶" "▷")
  ;;                                 (when (and unread-count (not (zerop unread-count)))
  ;;                                   (concat " " (number-to-string unread-count)))
  ;;                                 " ")
  ;;                         'face `(:inherit ,(if online-p 'success 'warning) :inverse-video t))))))
  (defun +tab-bar-telega-icon-update (&rest rest)
  "Update the Telega icon in the tab bar with notification counts.
Displays counts for:
- Unread unmuted messages (excluding mentions)
- Mentions
- Unread reactions
Uses nerd-icons for the Telegram icon and applies appropriate faces."
  (setq +tab-bar-telega-indicator-cache
        (when (and (fboundp 'telega-server-live-p)
                   (telega-server-live-p)
                   (buffer-live-p telega-server--buffer))
          (let* ((me-user (telega-user-me 'locally))
                 (online-p (and me-user (telega-user-online-p me-user)))
                 ;; reactions
                 (reactions-chats (telega-filter-chats (telega-chats-list) 
                                                       '(and is-known unread-reactions)))
                 (reactions-count (apply '+ (mapcar (telega--tl-prop :unread_reaction_count) 
                                                    reactions-chats)))
                 ;; mentioned
                 (mentioned-chats (telega-filter-chats (telega-chats-list) '(mention)))
                 (mentioned-count (apply '+ (mapcar (telega--tl-prop :unread_mention_count) 
                                                    mentioned-chats)))
                 ;; unread
                 (unmuted-count (or (plist-get telega--unread-chat-count :unread_unmuted_count) 0))
                 (mentioned-unmuted-chats (telega-filter-chats (telega-chats-list) 
                                                               '(and mention unmuted)))
                 (true-unmuted-count (- unmuted-count (length mentioned-unmuted-chats)))
                 (text (propertize (concat " " telega-symbol-telegram " "
                                           (when (> true-unmuted-count 0)
                                             (propertize (concat "●" (number-to-string true-unmuted-count) " ")
                                                         'face 'telega-unmuted-count))
                                           (when (> mentioned-count 0)
                                             (propertize (concat "@" (number-to-string mentioned-count) " ")
                                                         'face 'telega-mention-count))
                                           (when (> reactions-count 0)
                                             (propertize (concat "❤" (number-to-string reactions-count) " ")
                                                         'face 'telega-mention-count)))))
                 (first-name (plist-get me-user :first_name))
                 (last-name (plist-get me-user :last_name))
                 (help-echo (concat "Current User: " first-name " " last-name "\n"
                                    "Status: " (if online-p "online" "offline"))))
            `((tab-bar-persp menu-item
                             ,text
                             ignore
                             :help ,help-echo))))))

  ;; (defun +tab-bar-telega-icon ()
  ;;   (or +tab-bar-telega-indicator-cache
  ;;       (+tab-bar-telega-icon-update)))
  (defun +tab-bar-telega-icon ()
    (when (and (fboundp 'telega-server-live-p)
               (telega-server-live-p))
      (or +tab-bar-telega-indicator-cache
          (+tab-bar-telega-icon-update))))

  (advice-add 'telega--on-updateUnreadChatCount :after #'+tab-bar-telega-icon-update)
  (defun +hide-tab-bar ()
    (interactive)
    (setq tab-bar-format nil))

  (defun +show-tab-bar ()
    (interactive)
    (setq tab-bar-format '(+tab-bar-telega-icon
			   ;; meow-indicator
			   tab-bar-format-tabs tab-bar-separator))
    (tab-bar--update-tab-bar-lines))
  (+show-tab-bar)

  ;; 隐藏 org roam 文件的前缀
  ;; ( tab-bar-tab-name-function )

  ;; tab bar svg from @Eli
  ;; (defface tab-bar-svg-active
  ;;   '((t (:family "BlexMono Nerd Font Mono" :foreground "#a1aeb5"
  ;;                 :box (:line-width (4 . 5)
  ;;                                   :color "#a1aeb5"
  ;;                                   :style flat-button))))
  ;;   "Tab bar face for selected tab.")

  (defface tab-bar-svg-active
    '((t (:family "BlexMono Nerd Font Mono" :foreground "#a1aeb5")))
    "Tab bar face for selected tab.")

  (defface tab-bar-svg-inactive
    '((t (:family "BlexMono Nerd Font Mono" :foreground "#cad7de")))
    "Tab bar face for inactive tabs.")

  (defun eli/tab-bar-svg-padding (width string)
    (let* ((style svg-lib-style-default)
           (margin      (plist-get style :margin))
           (txt-char-width  (window-font-width nil 'fixed-pitch))
           (tag-width (- width (* margin txt-char-width)))
           (padding (- (/ tag-width txt-char-width) (length string))))
      padding))

  (defun svg-pad (p)
    "Normalize PADDING for svg-lib: number or cons."
    (cond
     ((numberp p) (max 0 p))
     ((and (consp p) (numberp (car p)) (numberp (cdr p)))
    (cons (max 0 (car p)) (max 0 (cdr p))))
     (t 2)))  ;; 默认 2px

  (defun eli/tab-bar-tab-name-with-svg (tab i)
    (let* ((current-p (eq (car tab) 'current-tab))
           (name (concat (if tab-bar-tab-hints (format "%d " i) "")
			 (alist-get 'name tab)
			 (or (and tab-bar-close-button-show
                                  (not (eq tab-bar-close-button-show
                                           (if current-p 'non-selected 'selected)))
                                  tab-bar-close-button)
                             "")))
           (name (replace-regexp-in-string "\\]" "］"
                                           (replace-regexp-in-string "\\[" "［" name)))
           (padding (plist-get svg-lib-style-default :padding))
           (width))
      (when tab-bar-auto-width
	(setq width (/ (frame-inner-width)
                       (length (funcall tab-bar-tabs-function))))
	(when tab-bar-auto-width-min
          (setq width (max width (if (window-system)
                                     (nth 0 tab-bar-auto-width-min)
                                   (nth 1 tab-bar-auto-width-min)))))
	(when tab-bar-auto-width-max
          (setq width (min width (if (window-system)
                                     (nth 0 tab-bar-auto-width-max)
                                   (nth 1 tab-bar-auto-width-max)))))
	(setq padding (eli/tab-bar-svg-padding width name)))
      (propertize
       name
       'display
       (svg-tag-make
	name
	:face (if (eq (car tab) 'current-tab) 'tab-bar-svg-active 'tab-bar-svg-inactive)
	:inverse t :margin 1 :radius 6 :padding (svg-pad padding)))))

  (setq tab-bar-tab-name-format-function #'eli/tab-bar-tab-name-with-svg)
  )

(use-package tab-line-mode
  :disabled t
  :init
  (global-tab-line-mode 1))

(use-package beacon
  :straight t
  :config
  (setq beacon-mode t))

;; (use-package highlight-indent-guides
;;   :straight t
;;   :hook
;;   (prog-mode . highlight-indent-guides-mode)
;;   :config
;;   (setq highlight-indent-guides-method 'character))

;;; 连续调整透明度
(use-package transwin
  :straight t
  :init
  (setq transwin-delta-alpha 15)
  (setq transwin-parameter-alpha 'alpha-background)
  (transwin-ask 100)
  :bind
  ("C-c C-=" . transwin-inc)
  ("C-c C--" . transwin-dec)
  ("C-c C-0" . transwin-toggle))

(use-package beacon
  :disabled t
  :straight t
  :demand t
  :config
  ; https://github.com/Malabarba/beacon/issues/46
  ; the value is compared with >, not >=
  (setq beacon-blink-when-point-moves-vertically 1)
  ; disable beacon for horizontal movement
  (setq beacon-blink-when-point-moves-horizontally 1)
  ; remove specific symbols from the variable 'beacon-dont-blink-commands
  (setq beacon-dont-blink-commands '(forward-char backward-char meow-right meow-left))
  (beacon-mode 1))

;;; 果冻光标
(use-package EmacsMacPluginModule
  :disabled
  :straight (:host github :repo "happyo/EmacsMacPluginModule")
  :init
  (require 'mac-plugin)
  (setq macos-project-root "~/.emacs.d/straight/repos/EmacsMacPluginModule/")
  ;; macos-module-build-release
  :config
  (mac-plugin-load-release)
  (atmosphere-enable)
  (mac-plugin-set-cursor-color "#fcc800")
  (mac-plugin-set-shadow-opacity 0.5)
  )

;;; 跳转后闪烁光标
(use-package pulse
  :custom-face
  (pulse-highlight-start-face ((t (:inherit region :background unspecified))))
  (pulse-highlight-face ((t (:inherit region :background unspecified :extend t))))
  :hook (((dumb-jump-after-jump imenu-after-jump) . +recenter-and-pulse)
         ((bookmark-after-jump magit-diff-visit-file next-error) . +recenter-and-pulse-line))
  :init
  (setq pulse-delay 0.1
        pulse-iterations 2)

  (defun +pulse-momentary-line (&rest _)
    "Pulse the current line."
    (pulse-momentary-highlight-one-line (point)))

  (defun +pulse-momentary (&rest _)
    "Pulse the region or the current line."
    (if (fboundp 'xref-pulse-momentarily)
        (xref-pulse-momentarily)
      (+pulse-momentary-line)))

  (defun +recenter-and-pulse(&rest _)
    "Recenter and pulse the region or the current line."
    (recenter)
    (+pulse-momentary))

  (defun +recenter-and-pulse-line (&rest _)
    "Recenter and pulse the current line."
    (recenter)
    (+pulse-momentary-line))

  (dolist (cmd '(recenter-top-bottom
                 other-window switch-to-buffer
                 aw-select toggle-window-split
                 windmove-do-window-select
                 pager-page-down pager-page-up
                 treemacs-select-window
                 tab-bar-select-tab))
    (advice-add cmd :after #'+pulse-momentary-line))

  (dolist (cmd '(pop-to-mark-command
                 pop-global-mark
                 goto-last-change))
    (advice-add cmd :after #'+recenter-and-pulse))

  (dolist (cmd '(symbol-overlay-basic-jump
                 compile-goto-error))
    (advice-add cmd :after #'+recenter-and-pulse-line))
  )

;;; 高亮上次编辑的位置
(use-package goggles
  :straight t
  :hook ((prog-mode text-mode) . goggles-mode)
  :config
  (setq-default goggles-pulse nil)
  )

;;; 光标所在括号高亮
(use-package paren
  :custom-face (show-paren-match ((t (:foreground "SpringGreen3" :underline t :weight bold))))
  :config
  (setq show-paren-when-point-inside-paren t
        show-paren-when-point-in-periphery t
        show-paren-context-when-offscreen t
        show-paren-delay 0.2)
  )

(use-package highlight-parentheses
  :straight t
  :hook ((minibuffer-setup . highlight-parentheses-minibuffer-setup)
         (prog-mode . highlight-parentheses-mode))
  :config
  (setq highlight-parentheses-colors '("firebrick1" "firebrick3" "orange1" "orange3")
        highlight-parentheses-attributes '((:underline t) (:underline t) (:underline t))
        highlight-parentheses-delay 0.2)
  )

;;; 彩虹括号
;; [rainbow-delimiters] Highlight brackets according to their depth
(use-package rainbow-delimiters
  :straight t
  :hook ((prog-mode conf-mode yaml-mode) . rainbow-delimiters-mode)
  :config
  (setq rainbow-delimiters-max-face-count 5))

;;; nyan-mode
(use-package nyan-mode
  :disabled
  :straight (:repo "zakudriver/nyan-mode")
  :hook
  (after-init . nyan-mode)
  :custom
  (nyan-bar-length       40)
  (nyan-animate-nyancat  t)
  (nyan-wavy-trail       t)
  (nyan-animation-frames 1))

(provide 'init-themes)
;;;init-themes.el ends here
