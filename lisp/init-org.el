(require-package 'org-superstar)
(require-package 'org-modern)
(require-package 'valign)
(require-package 'org-roam)
(require-package 'emacsql-sqlite-builtin)
(require-package 'org-roam-ui)
(require-package 'org-ref)
(require-package 'writeroom-mode)
(require-package 'org-variable-pitch)
(require-package 'color)
(require-package 'visual-fill-column)
(require-package 'ob-rust)
(require-package 'emacsql-sqlite-builtin)
(require-package 'xenops)

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

(use-package org-modern
  ;; :defer t
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
(use-package org-roam
  ;; :defer t
  :config
  (setq org-roam-directory "~/Documents/orgs")  ;; roam 应用的文件夹
  ;; (add-hook 'after-init-hook 'org-roam-mode)
  (setq org-roam-completion-system 'ivy) ;;使用ivy提示
  :custom
  (org-roam-database-connector 'sqlite-builtin))

;; whitespace mode 下显示零宽空格
(with-eval-after-load 'whitespace
  (setq whitespace-space-regexp "\\( +\\|\u200b\\)")
  (add-to-list 'whitespace-display-mappings '(space-mark #x200b [?▾])))

(use-package writeroom
  ;; :defer t

  :init
  (setq writeroom-fullscreen-effect 'maximized)

  :hook
  (org-mode . writeroom-mode)
  (writeroom-mode . variable-pitch-mode)
  (writeroom-mode . visual-line-mode)
  (visual-line-mode . visual-fill-column-mode)

  :custom
  (writeroom-mode-line t))

(use-package org-bars
  :config
  (add-hook 'org-mode-hook #'org-bars-mode))

(setq org-bars-stars '(:empty "◉"
			      :invisible "▸"
			      :visible "▾"))
(setq org-ellipsis "⤵")

;; agenda settings
(load "~/.emacs.d/site-lisp/next-spec-day.el")
(setq org-agenda-files '("~/Documents/orgs/tasks/"))

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
(define-key org-mode-map (kbd "H-SPC")
      (lambda () (interactive) (insert "\u200b")))

(defvar org-babel-python-command "python3.10")

;; xenops
(add-hook 'latex-mode-hook #'xenops-mode)
(add-hook 'LaTeX-mode-hook #'xenops-mode)
(add-hook 'org-mode-hook #'xenops-mode)
(setq xenops-cache-directory "~/Documents/orgs/ltximg/")
(setq xenops-font-height 160)
(setq xenops-font-height-code 140)
(setq xenops-math-image-scale-factor 1.7)
(setq xenops-reveal-on-entry t)
(defun xenops-math-reveal-alt (element)
    (xenops-element-overlays-delete element)
    (if current-prefix-arg
        (delete-file (xenops-math-get-cache-file element)))
    (let ((element-type (plist-get element :type))
          (begin-content (plist-get element :begin-content)))
      (goto-char (if (eq element-type 'block-math)
                     (1+ begin-content)
                   begin-content))))
  (advice-add #'xenops-math-reveal :override #'xenops-math-reveal-alt)

;; 公式编号
(defun eli/xenops-renumber-environment (orig-func element latex colors
                                                    cache-file display-image)
    (let ((results '())
          (counter -1)
          (numberp))
      (setq results (cl-loop for (begin .  env) in
                             (org-element-map (org-element-parse-buffer)
                                 'latex-environment
                               (lambda (env)
                                 (cons
                                  (org-element-property :begin env)
                                  (org-element-property :value env))))
                             collect
                             (cond
                              ((and (string-match "\\\\begin{equation}" env)
                                    (not (string-match "\\\\tag{" env)))
                               (cl-incf counter)
                               (cons begin counter))
                              ((and (string-match "\\\\begin{align}" env)
                                    (string-match "\\\\notag" env))
                               (cl-incf counter)
                               (cons begin counter))
                              ((string-match "\\\\begin{align}" env)
                               (prog2
                                   (cl-incf counter)
                                   (cons begin counter)
                                 (with-temp-buffer
                                   (insert env)
                                   (goto-char (point-min))
                                   ;; \\ is used for a new line. Each one leads
                                   ;; to a number
                                   (cl-incf counter (count-matches "\\\\$"))
                                   ;; unless there are nonumbers.
                                   (goto-char (point-min))
                                   (cl-decf counter
                                            (count-matches "\\nonumber")))))
                              (t
                               (cons begin nil)))))
      (when (setq numberp (cdr (assoc (plist-get element :begin) results)))
        (setq latex
              (concat
               (format "\\setcounter{equation}{%s}\n" numberp)
               latex))))
    (funcall orig-func element latex colors cache-file display-image))
  (advice-add 'xenops-math-latex-create-image :around #'eli/xenops-renumber-environment)

(provide 'init-org)
