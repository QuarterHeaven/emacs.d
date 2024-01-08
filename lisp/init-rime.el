(use-package rime
  :straight  (rime :type git
                   :host github
                   :repo "DogLooksGood/emacs-rime"
                   :files ("*.el" "Makefile" "lib.c"))
  :config
  (cond (sys/macp (setq rime-librime-root "~/.emacs.d/librime/dist"))
	(sys/linuxp (setq rime-librime-root (substring (shell-command-to-string "nix eval nixpkgs#librime.outPath") 1 -2)
			  rime-emacs-module-header-root (file-truename (concat (file-name-directory (directory-file-name (file-truename invocation-directory))) "include")))))
  (setq rime-user-data-dir "~/Library/Rime/")
  (setq rime-cursor "˰")
  ;; (setq rime-show-candidate 'minibuffer)
  (setq rime-show-candidate 'posframe)
  ;; 默认值
  (setq rime-translate-keybindings
	'("C-f" "C-b" "C-n" "C-p" "C-g" "<left>" "<right>" "<up>" "<down>" "<prior>" "<next>" "<delete>"))
  (define-key rime-mode-map (kbd "C-`") 'rime-send-keybinding)
;;; 提示当前的临时英文状态。具体参考 mode-line-mule-info 默认值，其中可能有其它有用信息
  (setq mode-line-mule-info '((:eval (rime-lighter))))
  (setq rime-disable-predicates
	'(rime-predicate-meow-normal-mode-p
	  rime-predicate-after-alphabet-char-p
	  rime-predicate-prog-in-code-p)
	)
  ;; (setq rime-inline-ascii-trigger 'shift-l)
  ;; (setq rime-inline-predicates
  ;; 	'(rime-predicate-space-after-cc-p ; 中文接一个空格的后面
  ;;         rime-predicate-current-uppercase-letter-p)) ; 当前输入是大写字母
  (defun rime-predicate-meow-normal-mode-p ()
    "Detect whether the current buffer is in `meow-normal-mode' state.

Can be used in `rime-disable-predicates' and `rime-inline-predicates'."
    (and (fboundp 'meow-mode)
	 (or (meow-normal-mode-p)
	     (meow-beacon-mode-p)
	     (meow-keypad-mode-p))))
  :custom
  (default-input-method "rime")
  (setq rime-cursor "˰"))

(use-package phi-search
  :straight t
  :init
  (global-set-key (kbd "C-s") 'phi-search)
  (global-set-key (kbd "C-r") 'phi-search-backward))

;;; 光标自动变色
;; (with-eval-after-load 'rime
;; (require 'im-cursor-chg)
;; (cursor-chg-mode 1)
;; )

(provide 'init-rime)
