(require-package 'org-superstar)
(require-package 'org-modern)
(require-package 'valign)
(require-package 'org-roam)
(require-package 'org-roam-ui)
(require-package 'org-ref)
(require-package 'writeroom-mode)
(require-package 'org-variable-pitch)
(require-package 'color)

					; (add-hook 'org-mode-hook (lambda () (org-superstar-mode 1)))
					; (add-hook 'org-mode-hook 'variable-pitch-mode)
(add-hook 'org-mode-hook 'org-variable-pitch-minor-mode)

(add-hook 'org-mode-hook (lambda() 
  (setq prettify-symbols-alist
                      '(("lambda"  . ?λ)
                        (":PROPERTIES:" . ?)
                        (":ID:" . ?)
                        (":END:" . ?)
                        ("#+TITLE:" . ?)
                        ("#+AUTHOR" . ?)
                        ("#+RESULTS:" . ?)
                        ("[ ]" . ?)
                        ("[-]" . ?)
                        ("[X]" . ?)
                        ("[#A]" . ?🅐)
                        ("[#B]" . ?🅑)
                        ("[#C]" . ?🅒)
                        ("#+BEGIN_SRC" . "λ")  ; previously ✎
                        ("#+END_SRC" . "□")
                        ("#+begin_src" . "λ")
                        ("#+end_src" . "□")
                        ("#+begin_quote" . ?»)
                        ("#+end_quote" . ?«)
                        ("#+BEGIN_QUOTE" . ?»)
                        ("#+END_QUOTE" . ?«)))
  (prettify-symbols-mode)))

(setq org-hide-emphasis-markers t)

(use-package org-modern
  :custom
  ;; Org modern settings
  (org-modern-star nil)
  (org-modern-priority nil)
  (org-modern-list nil)
  (org-modern-checkbox nil)
  (org-modern-todo t)
  (org-modern-keyword nil)
  (org-modern-block nil)

  ;; Editor settings
  (org-auto-align-tags nil)
  ; (org-tags-column 0)
  (org-catch-invisible-edits 'show-and-error)
  (org-special-ctrl-a/e t)
 
  :config
  ;(global-org-modern-mode 1)
  )

(setq modus-themes-org-blocks 'gray-background)
(add-hook 'org-mode-hook #'valign-mode)
(add-hook 'org-mode-hook #'toggle-truncate-lines)

(require 'org-tempo)

;; org roam settings
(setq org-roam-directory "~/Documents/orgs")  ;; roam 应用的文件夹
;; (add-hook 'after-init-hook 'org-roam-mode)

(setq org-roam-completion-system 'ivy) ;;使用ivy提示

;; 消灭中文强调的空格
(font-lock-add-keywords 'org-mode
                        '(("\\cc\\( \\)[/+*_=~][^a-zA-Z0-9/+*_=~\n]+?[/+*_=~]\\( \\)?\\cc?"
                           (1 (prog1 () (compose-region (match-beginning 1) (match-end 1) ""))))
                          ("\\cc?\\( \\)?[/+*_=~][^a-zA-Z0-9/+*_=~\n]+?[/+*_=~]\\( \\)\\cc"
                           (2 (prog1 () (compose-region (match-beginning 2) (match-end 2) "")))))
                        'append)

(use-package writeroom
  :hook
  (org-mode . writeroom-mode)
  :custom
  (writeroom-mode-line t)
  )

(require 'org-bars)
(add-hook 'org-mode-hook #'org-bars-mode)

(setq org-bars-stars '(:empty "◉"
                       :invisible "▸"
                       :visible "▾"))

;; agenda settings
(setq org-agenda-files '("~/Documents/orgs/tasks/"))

(provide 'init-org)
