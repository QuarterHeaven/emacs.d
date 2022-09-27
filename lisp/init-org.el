(require-package 'org-superstar)
(require-package 'org-modern)
(require-package 'valign)

; (add-hook 'org-mode-hook (lambda () (org-superstar-mode 1)))

(add-hook 'org-mode-hook (lambda() 
  (setq prettify-symbols-alist
                      '(("lambda"  . ?λ)
                        (":PROPERTIES:" . ?)
                        (":ID:" . ?)
                        (":END:" . ?)
                        ("#+TITLE:" . ?)
                        ("#+AUTHOR" . ?)
                        ("#+BEGIN_QUOTE" . ?)
                        ("#+END_QUOTE" . ?)
                        ("#+RESULTS:" . ?)
                        ("[ ]" . ?)
                        ("[-]" . ?)
                        ("[X]" . ?)
                        ("[#A]" . ?🅐)
                        ("[#B]" . ?🅑)
                        ("[#C]" . ?🅒)))
  (prettify-symbols-mode)))

(setq org-hide-emphasis-markers t)

(use-package org-modern
  :custom
  ;; Org modern settings
  ;; (org-modern-star nil)
  (org-modern-priority nil)
  (org-modern-list nil)
  (org-modern-checkbox nil)
  (org-modern-todo t)
  (org-modern-keyword nil)

  ;; Editor settings
  (org-auto-align-tags nil)
  (org-tags-column 0)
  (org-catch-invisible-edits 'show-and-error)
  (org-special-ctrl-a/e t)
  :config
  (global-org-modern-mode 1))

(setq modus-themes-org-blocks 'gray-background)
(add-hook 'org-mode-hook #'valign-mode)

(provide 'init-org)
