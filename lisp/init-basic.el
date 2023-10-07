;; 关闭自动保存
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq create-lockfiles nil)


(setq url-gateway-method 'socks)
(setq socks-server '("Default server" "127.0.0.1" 1081 5))

(setq url-gateway-local-host-regexp
      (concat "\\`" (regexp-opt '("localhost" "127.0.0.1")) "\\'"))

(setq truncate-lines t)

; 当前窗口打开 dired
(put 'dired-find-alternate-file 'disabled nil)

; 默认窗口最大化
;; (setq initial-frame-alist (quote ((fullscreen . maximize))))
;; (setq default-frame-alist (quote ((fullscreen . maximize))))
; (add-hook 'window-setup-hook #'toggle-frame-maximized)

; 隐藏开始界面
(setq inhibit-startup-message t)

; 关闭提示音
(setq ring-bell-function 'ignore)

; 关闭 native-comp warning
(setq native-comp-async-report-warnings-errors nil)

; 当前行高亮
(global-hl-line-mode)

; gpg
(setq epa-pinentry-mode 'loopback)

(global-set-key (kbd "C-c v") 'split-window-vertically)
(global-set-key (kbd "C-c h") 'split-window-horizontally)

(setq tab-bar-show 1)

(setq use-short-answers t)

(use-package pixel-scroll
  :config (pixel-scroll-precision-mode t))

; 编码设置
(set-language-info
     "UTF-8"
     'coding-priority
     '(utf-8 gb18030 gbk gb2312 iso-2022-cn chinese-big5 chinese-iso-8bit iso-8859-1))
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

(provide 'init-basic)
