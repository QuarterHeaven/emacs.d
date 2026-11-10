;;; init-notify.el ---                               -*- lexical-binding: t; -*-

;; Copyright (C) 2025  TakaObsid

;; Author: TakaObsid <takaobsid@Leyline>
;; Keywords:

(use-package knockknock
  :straight (:host github :repo "konrad1977/knockknock")
  :custom
  ;; Duration
  (knockknock-default-duration 5)

  ;; Colors (nil = use theme defaults)
  (knockknock-background-color nil)  ; Use theme background
  (knockknock-foreground-color nil)  ; Use theme foreground
  (knockknock-border-color "orange")

  (knockknock-border-width 2)
  (knockknock-left-fringe 10)
  (knockknock-right-fringe 10)

  (knockknock-use-icons t)                ; Enable/disable icons
  (knockknock-default-icon "fa-bell")     ; Default icon

  (knockknock-max-message-width 40)       ; Max characters per line

  (knockknock-poshandler #'posframe-poshandler-window-top-right-corner)

  (knockknock-use-svg-layout t)           ; SVG layout (default)
  (knockknock-svg-icon-size 32)           ; Icon size in pixels
  (knockknock-svg-width 300)              ; Canvas width in pixels
  (knockknock-svg-padding 12)             ; Padding between icon and text
  (knockknock-max-image-size 10000)       ; Max image size (set automatically)
  :init
  ;; 文件保存通知
  (defun my-save-notification ()
    (knockknock-notify :title "文件已保存"
                       :message (buffer-name)
                       :icon "fa-save"
                       :duration 2))

  ;; 代码运行完成通知
  (defun my-quickrun-notification ()
    (knockknock-notify :title "代码运行完成"
                       :message (format "%s 执行完毕" (buffer-name))
                       :icon "fa-play-circle"
                       :duration 3))

  ;; Git commit 通知
  (defun my-git-commit-notification ()
    (knockknock-notify :title "Git 提交完成"
                       :message "更改已成功提交"
                       :icon "fa-git"
                       :duration 3))

  ;; Git push 通知
  (defun my-git-push-notification ()
    (knockknock-notify :title "Git Push 完成"
                       :message "代码已推送到远程仓库"
                       :icon "fa-cloud-upload"
                       :duration 3))

  ;; Git pull 通知
  (defun my-git-pull-notification ()
    (knockknock-notify :title "Git Pull 完成"
                       :message "已从远程仓库拉取更新"
                       :icon "fa-cloud-download"
                       :duration 3))

  ;; AI 请求完成通知
  (defun my-gptel-notification ()
    (knockknock-notify :title "AI 响应完成"
                       :message "gptel 请求已完成"
                       :icon "fa-robot"
                       :duration 3))

  ;; AI 重命名完成通知
  (defun my-gptel-rename-notification ()
    (knockknock-notify :title "聊天重命名完成"
                       :message "已使用 AI 建议的名称"
                       :icon "fa-edit"
                       :duration 2))

  ;; LSP 连接成功通知
  (defun my-lsp-connected-notification (server)
    (knockknock-notify :title "LSP 已连接"
                       :message (format "语言服务器连接成功")
                       :icon "fa-plug"
                       :duration 2))

  ;; LSP 连接失败通知
  (defun my-lsp-failed-notification (server)
    (knockknock-notify :title "LSP 连接失败"
                       :message (format "语言服务器 %s 无法连接"
                                       (if (listp server) (car server) server))
                       :icon "fa-exclamation-triangle"
                       :duration 4))

  ;; 代码格式化完成通知
  (defun my-format-notification ()
    (knockknock-notify :title "代码格式化完成"
                       :message (buffer-name)
                       :icon "fa-align-left"
                       :duration 2))

  ;; 编译完成通知
  (defun my-compile-notification (status)
    (if (eq status 0)
        (knockknock-notify :title "编译成功"
                          :message "编译过程无错误"
                          :icon "fa-check-circle"
                          :duration 3)
      (knockknock-notify :title "编译失败"
                        :message (format "编译返回错误代码: %s" status)
                        :icon "fa-times-circle"
                        :duration 4)))

  ;; 调试器启动通知
  (defun my-dape-start-notification ()
    (knockknock-notify :title "调试会话已启动"
                       :message "Dape 调试器正在运行"
                       :icon "fa-bug"
                       :duration 2))

  ;; 调试器停止通知
  (defun my-dape-stop-notification ()
    (knockknock-notify :title "调试会话已结束"
                       :message "Dape 调试器已停止"
                       :icon "fa-stop-circle"
                       :duration 2))

  ;; 包安装完成通知
  (defun my-package-install-notification (package)
    (knockknock-notify :title "包安装完成"
                       :message (format "已安装 %s" package)
                       :icon "fa-download"
                       :duration 3))

  :hook
  (after-save . my-save-notification)

  :config
  ;; ============= 代码运行与编译通知 =============
  ;; quickrun 运行完成通知
  (with-eval-after-load 'quickrun
    (defadvice! +quickrun-notify-after-execute-a (&rest _)
      "在 quickrun 执行完成后显示通知"
      :after #'quickrun
      (run-with-timer 0.5 nil #'my-quickrun-notification)))

  ;; vterm 编译完成通知
  (with-eval-after-load 'vterm
    (defun my-vterm-compile-with-notification ()
      "带通知的编译函数"
      (interactive)
      (vterm-compile)
      (run-with-timer 2 nil
                      (lambda ()
                        (my-compile-notification 0)))))

  ;; ============= Git 操作通知 =============
  (with-eval-after-load 'magit
    ;; Git commit 通知
    (defadvice! +magit-commit-notify-a (&rest _)
      "Git commit 完成后显示通知"
      :after #'magit-commit-create
      (my-git-commit-notification))

    ;; Git push 通知
    (defadvice! +magit-push-notify-a (&rest _)
      "Git push 完成后显示通知"
      :after #'magit-push-current-to-upstream
      (my-git-push-notification))

    ;; Git pull 通知
    (defadvice! +magit-pull-notify-a (&rest _)
      "Git pull 完成后显示通知"
      :after #'magit-pull-from-upstream
      (my-git-pull-notification)))

  ;; ============= AI 助手通知 =============
  (with-eval-after-load 'gptel
    ;; gptel 请求完成通知
    (add-hook 'gptel-post-response-functions
              (lambda (&rest _)
                (my-gptel-notification))))

  ;; ============= LSP 通知 =============
  (with-eval-after-load 'eglot
    ;; LSP 连接成功通知（暂时禁用，因为与 Java LSP 有兼容性问题）
    ;; (add-hook 'eglot-managed-mode-hook
    ;;           (lambda ()
    ;;             (when (eglot-current-server)
    ;;               (run-with-timer 0.5 nil
    ;;                               (lambda ()
    ;;                                 (ignore-errors
    ;;                                   (when (eglot-current-server)
    ;;                                     (my-lsp-connected-notification nil))))))))

    ;; 代码格式化完成通知
    (defadvice! +eglot-format-notify-a (&rest _)
      "代码格式化完成后显示通知"
      :after #'eglot-format-buffer
      (my-format-notification)))

  ;; ============= 调试器通知 =============
  (with-eval-after-load 'dape
    ;; 调试会话启动通知
    (add-hook 'dape-on-start-hooks #'my-dape-start-notification)

    ;; 调试会话停止通知
    (add-hook 'dape-on-stopped-hooks
              (lambda ()
                (run-with-timer 0.2 nil #'my-dape-stop-notification))))

  ;; ============= 包管理通知 =============
  (with-eval-after-load 'straight
    ;; 包安装完成通知（可选，可能比较吵）
    ;; (defadvice! +straight-install-notify-a (package &rest _)
    ;;   "包安装完成后显示通知"
    ;;   :after #'straight-use-package
    ;;   (my-package-install-notification package))
    ))

(provide 'init-notify)
