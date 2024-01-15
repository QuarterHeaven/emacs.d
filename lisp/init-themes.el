;;;init-themes.el---Defaultsforthemes-*-lexical-binding:t-*-
;;;Commentary:
;;;Code:

(use-package all-the-icons
  :straight t)

(setq inhibit-compacting-font-caches t)

;;Don't prompt to confirm theme safety. This avoids problems with
;;first-time startup on Emacs > 26.3.
(setq custom-safe-themes t)

;;If you don't customize it, this is the theme you get.
;; (setq-default custom-enabled-themes '(doom-city-light))
(use-package doom-themes
  :straight t
  :init
  (load-theme 'doom-bluloco-light t)
  ;; (load-theme 'doom-ayu-light t)
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled

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
   '(font-lock-comment-face ((t (:slant italic :family "Victor Mono"))))))

;; (load-theme 'doom-ayu-light t)
;; (load-theme 'doom-tomorrow-day t)

;;Ensure that themes will be applied even if they have not been customized
(defun reapply-themes ()
  "Forcibly load the themes listed in `custom-enabled-themes'."
  (dolist (theme custom-enabled-themes)
    (unless (custom-theme-p theme)
      (load-theme theme)))
  (custom-set-variables `(custom-enabled-themes (quote ,custom-enabled-themes))))

;; Toggle between light and dark

;; icons
(defconst sys/win32p
  (eq system-type 'windows-nt)
  "Are we running on a WinTel system?")

;; Show line numbers
(use-package display-line-numbers
  :straight t
  :hook ((prog-mode yaml-mode conf-mode) . display-line-numbers-mode)
  :init (setq display-line-numbers-width-start t))

;; Suppress GUI features
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

;; Smooth scrolling over images
(use-package iscroll
  :straight t
  :diminish
  :hook (image-mode . iscroll-mode))

;; Use fixed pitch where it's sensible
(use-package mixed-pitch
  :straight t
  :diminish)

;; Display ugly ^L page breaks as tidy horizontal lines
(use-package page-break-lines
  :straight t
  :diminish
  :hook (after-init . global-page-break-lines-mode))

;; Don't use GTK+ tooltip
(when (boundp 'x-gtk-use-system-tooltips)
  (setq x-gtk-use-system-tooltips nil))

;; Mode line
(use-package doom-modeline
  :straight t
  :hook (after-init . doom-modeline-mode)
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
	doom-modeline-github-interval (* 30 60))
  (set-face-attribute 'doom-modeline nil :inherit 'mode-line)
  (doom-modeline-def-segment image-info
    "Display the current image's info on the modeline"
    (concat
     doom-modeline-spc
     (when (eq major-mode 'image-mode)
       ;; Needs imagemagick installed.
       (process-lines "identify" "-format" "[%m %wx%h %b]" (buffer-file-name))))))

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
  :hook (doom-modeline-mode . minions-mode))

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

  :config
  (require 'svg-lib)
  (require 'svg-tag-mode)
  (setq tab-bar-separator ""
        ;; tab-bar-new-tab-choice "*dashboard*"
        tab-bar-tab-name-truncated-max 20
        tab-bar-auto-width nil
        tab-bar-close-button-show nil
        tab-bar-tab-hints t
	tab-bar-mode t)
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
    (when (buffer-live-p telega-server--buffer)
      (let* ((me-user (telega-user-me 'locally))
             (online-p (and me-user (telega-user-online-p me-user)))
             ;; reactions
             (reactions-chats (telega-filter-chats telega--ordered-chats '(and is-known unread-reactions)))
             (reactions-count (apply '+ (mapcar (telega--tl-prop :unread_reaction_count) reactions-chats)))
             ;; mentioned
             (mentioned-chats (telega-filter-chats telega--ordered-chats '(mention)))
             (mentioned-count (apply '+ (mapcar (telega--tl-prop :unread_mention_count) mentioned-chats)))
             ;; unread
             (unmuted-count (or (plist-get telega--unread-chat-count :unread_unmuted_count) 0))
             (mentioned-unmuted-chats (telega-filter-chats telega--ordered-chats '(and mention unmuted)))
             (true-unmuted-count (- unmuted-count (length mentioned-unmuted-chats)))
             (text (propertize (concat " " telega-symbol-telegram " "
                                       (when (> true-unmuted-count 0)
                                         (concat "●" (number-to-string true-unmuted-count) " "))
                                       (when (> mentioned-count 0)
                                         (concat "@" (number-to-string mentioned-count) " "))
                                       (when (> reactions-count 0)
                                         (concat "❤" (number-to-string reactions-count) " ")))
                               'face `(:inherit font-lock-keyword-face :inverse-video ,online-p)))
             (first-name (plist-get me-user :first_name))
             (last-name (plist-get me-user :last_name))
             (help-echo (concat "Current User: " first-name " " last-name "\n"
                                "Status: " (if online-p "online" "offline"))))
        (setq +tab-bar-telega-indicator-cache
              `((tab-bar-persp menu-item
                               ,text
                               ignore
                               :help ,help-echo))))
      (force-mode-line-update t)
      +tab-bar-telega-indicator-cache))

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
			   meow-indicator
			   tab-bar-format-tabs tab-bar-separator))
    (tab-bar--update-tab-bar-lines))
  (+show-tab-bar)

  ;; tab bar svg from @Eli
  (defface tab-bar-svg-active
    '((t (:family "BlexMono Nerd Font Mono" :foreground "#a1aeb5"
                  :box (:line-width (4 . 5)
                                    :color "#a1aeb5"
                                    :style flat-button))))
    "Tab bar face for selected tab.")

  (defvar eli/tab-bar-svg-padding-cache nil)

  (defun eli/tab-bar-svg-padding (width string)
    (unless eli/tab-bar-svg-padding-cache
      (setq eli/tab-bar-svg-padding-cache (make-hash-table :test #'equal)))
    (with-memoization (gethash (list width string)
                               eli/tab-bar-svg-padding-cache)
      (let* ((margin (plist-get svg-lib-style-default :margin))
             (txt-char-width  (window-font-width nil 'fixed-pitch))
             (tag-width (- width (* margin txt-char-width)))
             (padding (- (/ tag-width txt-char-width) (length string))))
	padding)))

  (defun eli/tab-bar-tab-name-with-svg (tab i)
    (let* ((current-p (eq (car tab) 'current-tab))
           (width (/ (frame-inner-width)
                     (length (funcall #'tab-bar-tabs))))
           (name (concat (if tab-bar-tab-hints (format "%d " i) "")
			 (alist-get 'name tab)
			 (or (and tab-bar-close-button-show
                                  (not (eq tab-bar-close-button-show
                                           (if current-p 'non-selected 'selected)))
                                  tab-bar-close-button)
                             ""))))
      (when tab-bar-auto-width-min
	(setq width (max width (if (window-system)
                                   (nth 0 tab-bar-auto-width-min)
				 (nth 1 tab-bar-auto-width-min)))))
      (when tab-bar-auto-width-max
	(setq width (min width (if (window-system)
                                   (nth 0 tab-bar-auto-width-max)
				 (nth 1 tab-bar-auto-width-max)))))
      (let ((continue t)
            (prev-width (string-pixel-width name))
            (padding (plist-get svg-lib-style-default :padding))
            curr-width)
	(cond
	 ((< prev-width width)
          (setq padding (eli/tab-bar-svg-padding width name)))
	 ((> prev-width width)
          (while continue
            (setf (substring name -1) "")
            (setq curr-width (string-pixel-width name))
            (if (and (> curr-width width)
                     (< curr-width prev-width))
		(setq prev-width curr-width)
              (setq continue nil)))))
	(propertize
	 name
	 'face 'tab-bar-tab-inactive
	 'display
	 (svg-tag-make
          name
          :face 'tab-bar-svg-active
          :inverse (eq (car tab) 'current-tab) :margin 0 :radius 6 :padding padding)))))

  ;;; 给 svg-tag-mode 添加缓存，对于同样的参数使用缓存中的图片
  (defvar eli/svg-tag-cache nil)

  (defun eli/svg-tag-with-cache (orig &rest args)
    (unless eli/svg-tag-cache
      (setq eli/svg-tag-cache (make-hash-table :test 'equal)))
    (with-memoization (gethash args eli/svg-tag-cache)
      (apply orig args)))

  (if (not (daemonp))
      (progn
	(setq tab-bar-tab-name-format-function #'eli/tab-bar-tab-name-with-svg)
	(advice-add #'svg-tag-make :around #'eli/svg-tag-with-cache)))

  ;; 隐藏 org roam 文件的前缀
  ;; ( tab-bar-tab-name-function )
  )

(use-package beacon
  :straight t
  :config
  (setq beacon-mode t))

(use-package highlight-indent-guides
  :straight t
  :hook
  (prog-mode . highlight-indent-guides-mode)
  :config
  (setq highlight-indent-guides-method 'character))

(provide 'init-themes)
;;;init-themes.el ends here
