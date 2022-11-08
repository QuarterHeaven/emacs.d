(require-package 'org-superstar)
(require-package 'org-modern)
(require-package 'valign)
(require-package 'org-roam)
(require-package 'org-roam-ui)
(require-package 'org-ref)
(require-package 'writeroom-mode)
(require-package 'org-variable-pitch)
(require-package 'color)
(require-package 'visual-fill-column)

					; (add-hook 'org-mode-hook (lambda () (org-superstar-mode 1)))
					; (add-hook 'org-mode-hook 'variable-pitch-mode)
;;(add-hook 'org-mode-hook 'org-variable-pitch-minor-mode)

(add-hook 'org-mode-hook (lambda() 
  (setq prettify-symbols-alist
                      '(("lambda"  . ?λ)
                        (":PROPERTIES:" . ?)
                        (":ID:" . ?)
                        (":END:" . ?)
                        ("#+TITLE:" . ?)
                        ("#+AUTHOR:" . ?)
                        ("#+RESULTS:" . ?)
			(":properties:" . ?)
                        (":id:" . ?)
                        (":end:" . ?)
                        ("#+title:" . ?)
                        ("#+author:" . ?)
                        ("#+results:" . ?)
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
  (writeroom-mode . variable-pitch-mode)
  (writeroom-mode . visual-line-mode)
  (visual-line-mode . visual-fill-column-mode)
  :custom
  (writeroom-mode-line t)
  )

(require 'org-bars)
(add-hook 'org-mode-hook #'org-bars-mode)

(setq org-bars-stars '(:empty "◉"
                       :invisible "▸"
                       :visible "▾"))
(setq org-ellipsis "⤵")

;; agenda settings
(load "~/.emacs.d/site-lisp/next-spec-day.el")
(setq org-agenda-files '("~/Documents/orgs/tasks/"))

(require-package 'ob-rust)
(org-babel-do-load-languages
      'org-babel-load-languages
      '((emacs-lisp . t)
        (C . t)
        (java . t)
        (js . t)
        (python . t)
        (shell . t)
        (latex . t)
	(rust . t)))

(setq word-wrap-by-category t)

(define-key org-mode-map (kbd "C-C <C-backspace>") 'org-mark-ring-goto)

(defvar org-babel-python-command "python3.10")

(provide 'init-org)
