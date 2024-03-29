(use-package magit
  :straight t
  :bind (("C-x g" . magit))
  :config
  (setq auth-sources '("~/.authinfo.gpg"))
  (magit-auto-revert-mode t)
  (setq auto-revert-use-notify nil)
  (defun magit-submodule-remove+ ()
    (interactive)
    (magit-submodule-remove (list (magit-read-module-path "Remove module")) "--force" nil)))

(use-package forge
             :straight t
  :after (magit)
  :defines forge-topic-list-columns
  :custom-face
  (forge-topic-label ((t (:inherit variable-pitch :height 0.9 :width condensed :weight regular :underline nil))))
  :init
  (setq forge-topic-list-columns
        '(("#" 5 forge-topic-list-sort-by-number (:right-align t) number nil)
          ("Title" 60 t nil title  nil)
          ("State" 6 t nil state nil)
          ("Updated" 10 t nil updated nil))))

(use-package diff-hl
  :straight t
  :hook ((magit-pre-refresh . diff-hl-magit-pre-refresh)
	 (magit-post-refresh . diff-hl-magit-post-refresh))
  :init
  (global-diff-hl-mode))

(provide 'init-magit)
