;; -*- lexical-binding: t -*-
;; 关闭自动保存
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq create-lockfiles nil)

;; (setq url-gateway-method 'socks)
;; (setq socks-server '("Default server" "127.0.0.1" 1081 5))

;; (setq url-gateway-local-host-regexp
;;       (concat "\\`" (regexp-opt '("localhost" "127.0.0.1")) "\\'"))

(setq url-proxy-services
      '(("no_proxy" . "^\\(localhost\\|10\\..*\\|192\\.168\\..*\\)")
	("socks5" . "127.0.0.1:1081")
        ("http" . "127.0.0.1:1081")
        ("https" . "127.0.0.1:1081")))

(setq truncate-lines t)

; 当前窗口打开 dired
(put 'dired-find-alternate-file 'disabled nil)

; 默认窗口最大化
;; (setq initial-frame-alist (quote ((fullscreen . maximize))))
;; (setq default-frame-alist (quote ((fullscreen . maximize))))
;; (add-hook 'window-setup-hook #'toggle-frame-maximized)
;; (when (daemonp)
;;   (add-hook 'server-after-make-frame-hook #'toggle-frame-maximized))

; 关闭提示音
(setq ring-bell-function 'ignore)

; 关闭 native-comp warning
(setq native-comp-async-report-warnings-errors nil)

; 当前行高亮
(use-package hl-line
  :hook (after-init . global-hl-line-mode)
  :config
  (setq hl-line-sticky-flag nil)
  ;; Highlight starts from EOL, to avoid conflicts with other overlays
  (setq hl-line-range-function (lambda () (cons (line-end-position)
                                           (line-beginning-position 2)))))

; gpg
(setq epa-pinentry-mode 'loopback)

(global-set-key (kbd "C-c v") 'split-window-vertically)
(global-set-key (kbd "C-c h") 'split-window-horizontally)

(setq use-short-answers t)

; 编码设置
(set-language-info
 "UTF-8"
 'coding-priority
 '(utf-8 gb18030 gbk gb2312 iso-2022-cn chinese-big5 chinese-iso-8bit))
    (prefer-coding-system 'cp950)
    (prefer-coding-system 'gb2312)
    (prefer-coding-system 'cp936)
    (prefer-coding-system 'gb18030)
    (prefer-coding-system 'utf-16)
    (prefer-coding-system 'utf-8-dos)
    (prefer-coding-system 'utf-8-unix)
    (prefer-coding-system 'utf-8)

;;(setq file-name-coding-system 'gb18030)

(setq default-buffer-file-conding-system 'utf-8)

;; disable title-bar; emacs >= 29 only
;; (cond (sys/macp
;;        (add-to-list 'default-frame-alist '(undecorated-round . t))))
;; (cond (sys/linuxp
(add-to-list 'default-frame-alist '(undecorated . t))
;; ))					

(add-hook 'window-setup-hook (lambda ()
			       (tool-bar-mode -1)
                                 (menu-bar-mode 0)
			       (setq inhibit-startup-message t)
			       (toggle-frame-maximized)
			       (scroll-bar-mode -1)
			       (setq scroll-conservatively 1)
			       ))

(setq-default bidi-display-reordering nil)
(setq bidi-inhibit-bpa t
      long-line-threshold 1000
      large-hscroll-threshold 1000
      syntax-wholeline-max 1000)

(setq-default indent-tabs-mode nil)

(provide 'init-basic)
