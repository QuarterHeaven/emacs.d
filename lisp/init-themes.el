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
  :init (load-theme 'doom-bluloco-light t)
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
  )

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

(use-package tab-bar
  :config
  (setq tab-bar-separator ""
        ;; tab-bar-new-tab-choice "*dashboard*"
        tab-bar-tab-name-truncated-max 20
        tab-bar-auto-width nil
        tab-bar-close-button-show nil
        tab-bar-tab-hints t
	tab-bar-mode t)
  (customize-set-variable 'tab-bar-select-tab-modifiers '(hyper))

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

  :hook
  (telega-connection-state-hook . +tab-bar-telega-icon-update)
  (telega-kill-hook . +tab-bar-telega-icon-update)

  :bind
  ("C-c n n" . tab-bar-switch-to-next-tab)
  ("C-c n p" . tab-bar-switch-to-prev-tab)
  ("C-c n t" . tab-bar-new-tab)
  ("C-c n w" . tab-bar-close-tab)
  )

(use-package beacon
  :straight t
  :config
  (setq beacon-mode t))

(provide 'init-themes)
;;;init-themes.el ends here
