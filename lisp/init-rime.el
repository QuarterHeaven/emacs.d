(require-package 'rime)
(require-package 'phi-search)

(defun rime-predicate-meow-normal-mode-p ()
  "Detect whether the current buffer is in `meow-normal-mode' state.

Can be used in `rime-disable-predicates' and `rime-inline-predicates'."
  (and (fboundp 'meow-mode)
       (or (meow-normal-mode-p))))

(use-package rime
  :config
  (setq rime-librime-root "~/.emacs.d/librime/dist")
  (setq rime-cursor "˰")
  (setq rime-show-candidate 'posframe)
  ;; 默认值
  (setq rime-translate-keybindings
	'("C-f" "C-b" "C-n" "C-p" "C-g" "<left>" "<right>" "<up>" "<down>" "<prior>" "<next>" "<delete>" "<shift>"))
  (define-key rime-mode-map (kbd "C-`") 'rime-send-keybinding)
;;; 提示当前的临时英文状态。具体参考 mode-line-mule-info 默认值，其中可能有其它有用信息
  (setq mode-line-mule-info '((:eval (rime-lighter))))
 (setq rime-disable-predicates
     '(rime-predicate-meow-normal-mode-p
;        rime-predicate-after-alphabet-char-p
;        rime-predicate-prog-in-code-p)
     ))
  :custom
  (default-input-method "rime")
  )

(global-set-key (kbd "C-s") 'phi-search)
(global-set-key (kbd "C-r") 'phi-search-backward)

(provide 'init-rime)
