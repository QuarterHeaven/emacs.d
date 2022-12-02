(require-package 'company)
(require-package 'company-box)

(setq company-global-modes '(not org-mode))
(add-hook 'after-init-hook 'global-company-mode)
(add-hook 'company-mode-hook 'company-box-mode)

(setq company-tooltip-align-annotations t ; 注释贴右侧对齐
        company-tooltip-limit 20            ; 菜单里可选项数量
        company-show-numbers t              ; 显示编号（然后可以用 M-数字 快速选定某一项）
        company-idle-delay .2               ; 延时多少秒后弹出
        company-minimum-prefix-length 1     ; 至少几个字符后开始补全
        )

(provide 'init-company)
