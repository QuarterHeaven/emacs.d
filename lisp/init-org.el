(add-to-list 'load-path "~/.emacs.d/site-lisp/org-mode")
(add-to-list 'load-path "~/.emacs.d/site-lisp/org-roam/extensions")
(require 'org-superstar)
(require 'org-modern)
(require 'valign)
(require 'org-roam)
(require 'emacsql-sqlite)
(require 'org-roam-ui)
(require 'org-ref)
(require 'writeroom-mode)
(require 'org-variable-pitch)
(require 'color)
(require 'visual-fill-column)
(require 'ob-rust)
(require 'emacsql-sqlite-builtin)
(require 'org-tempo)
(require 'org-zotxt)
(require 'zotxt)
(require 'org-appear)
(require 'org-visual-indent)
					; (add-hook 'org-mode-hook (lambda () (org-superstar-mode 1)))
					; (add-hook 'org-mode-hook 'variable-pitch-mode)
;;(add-hook 'org-mode-hook 'org-variable-pitch-minor-mode)


;; new org mode test
(setq org-latex-preview-auto-generate 'live
      org-latex-preview-debounce 0.2
      org-latex-preview-throttle 0.2
      org-latex-preview-numbered nil)
(plist-put org-latex-preview-options :zoom 1.1)
(add-hook 'org-mode-hook 'org-latex-preview-auto-mode)



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
				   ("- [ ]" . ?)
				   ("- [-]" . ?)
				   ("- [X]" . ?)
				   ("- [x]" . ?)
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
(add-hook 'org-mode-hook 'org-appear-mode)

(use-package org-modern
  :custom
  ;; Org modern settings
  (org-modern-priority nil)
  (org-modern-list nil)
  (org-modern-checkbox nil)
  (org-modern-todo t)
  (org-modern-keyword nil)
  (org-modern-block nil)
  (org-modern-star '("⚀" "⚁" "⚂" "⚃" "⚄" "⚅"))

  ;; Editor settings
  (org-auto-align-tags nil)
					; (org-tags-column 0)
  (org-catch-invisible-edits 'show-and-error)
  (org-special-ctrl-a/e t)

  :config
  (global-org-modern-mode 1)
  )

(setq modus-themes-org-blocks 'gray-background)
(add-hook 'org-mode-hook #'valign-mode)

;; org roam settings
(use-package org-roam
  :defer t
  :config
  ;; (add-hook 'after-init-hook 'org-roam-mode)
  (org-roam-db-autosync-enable)
  :custom
  (org-roam-database-connector 'sqlite-builtin)
  (org-roam-directory "~/Documents/orgs")  ;; roam 应用的文件夹
)

;; whitespace mode 下显示零宽空格
(with-eval-after-load 'whitespace
  (setq whitespace-space-regexp "\\( +\\|\u200b\\)")
  (add-to-list 'whitespace-display-mappings '(space-mark #x200b [?▾])))

;; (use-package writeroom
;;   :defer t

;;   :init
;;   (setq writeroom-fullscreen-effect 'maximized)

;;   :hook
;;   (org-mode . writeroom-mode)
;;   (writeroom-mode . variable-pitch-mode)
;;   (writeroom-mode . visual-line-mode)
;;   (visual-line-mode . visual-fill-column-mode)

;;   :custom
;;   (writeroom-mode-line t))
(use-package writeroom-mode
  :hook
  (after-init . global-writeroom-mode)
  (org-mode . variable-pitch-mode)
  (org-mode . visual-line-mode)
  (visual-line-mode . visual-fill-column-mode)
  
  :config
  (setq split-width-threshold 120

        writeroom-width 128
        writeroom-bottom-divider-width 0
        writeroom-fringes-outside-margins t
        writeroom-fullscreen-effect nil
        writeroom-major-modes '(text-mode prog-mode conf-mode special-mode Info-mode dired-mode)
        writeroom-major-modes-exceptions '(process-menu-mode proced-mode)
        writeroom-maximize-window nil
        writeroom-mode-line t
        writeroom-mode-line-toggle-position 'mode-line-format))

;; (use-package org-bars
;;   :config
;;   (add-hook 'org-mode-hook #'org-bars-mode))

;; (setq org-bars-stars '(:empty "◉"
;; 			      :invisible "▸"
;; 			      :visible "▾"))
(setq org-ellipsis "⤵")

;; agenda settings
(load "~/.emacs.d/site-lisp/next-spec-day.el")
(setq org-agenda-files '("~/Documents/orgs/agenda.org"))

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
(when (executable-find "jupyter")
  (add-to-list 'org-babel-load-languages '(jupyter . t) t))

(setq word-wrap-by-category t)

(define-key org-mode-map (kbd "C-C <C-backspace>") 'org-mark-ring-goto)
(define-key org-mode-map (kbd "H-SPC")
      (lambda () (interactive) (insert "\u200b")))

(setq org-babel-python-command "python3.10")

(with-eval-after-load 'org
  (add-to-list 'org-latex-default-packages-alist '("" "mathspec" t ("xelatex")))
  (add-to-list 'org-latex-default-packages-alist '("UTF8" "ctex" t ("xelatex"))))


;; org-xlatex
(use-package org-xlatex
  :after (org)
  :hook (org-mode . org-xlatex-mode))
(setq org-xlatex-position-indicator t)

;; org-capture
(setq org-default-notes-file "~/Documents/orgs/agenda.org")

;; org-zotxt
(add-hook 'org-mode-hook 'org-zotxt-mode)

;; org-visual-outline
(setq org-visual-indent-color-indent
      (cl-loop for x from 1 to 8
               with color = nil
               do (setq color (or (face-foreground
                                   (intern
                                    (concat "outline-"
                                            (number-to-string x))))
                                  (face-foreground 'org-level-1)))
               collect `(,x ,(list
                              :background color
                              :foreground color
                              :height .1))))
;; If you don’t have all of the org-level-x faces set, this will use
;; org-level-1 as the backup. Or figure out your own way of doing it.
;; None of it matters anyway.
(add-hook #'org-mode-hook 'org-visual-indent-mode)

(provide 'init-org)
