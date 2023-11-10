(use-package visual-fill-column
  :straight t)

(use-package org
  :defer
  :straight `(org
              :fork (:host nil
			   :repo "https://git.tecosaur.net/tec/org-mode.git"
			   :branch "dev"
			   :remote "tecosaur")
              :files (:defaults "etc")
              :build t
              :pre-build
              (with-temp-file "org-version.el"
		(require 'lisp-mnt)
		(let ((version
                       (with-temp-buffer
			 (insert-file-contents "lisp/org.el")
			 (lm-header "version")))
                      (git-version
                       (string-trim
			(with-temp-buffer
                          (call-process "git" nil t nil "rev-parse" "--short" "HEAD")
                          (buffer-string)))))
                  (insert
                   (format "(defun org-release () \"The release version of Org.\" %S)\n" version)
                   (format "(defun org-git-version () \"The truncate git commit hash of Org mode.\" %S)\n" git-version)
                   "(provide 'org-version)\n")))
              :pin nil)
  :hook
  ((org-mode . org-latex-preview-auto-mode)
   (org-mode . org-toggle-pretty-entities))
  :config
  (define-key org-mode-map (kbd "C-C <C-backspace>") 'org-mark-ring-goto)
  (define-key org-mode-map (kbd "H-SPC")
	      (lambda () (interactive) (insert "\u200b")))
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
  (setq org-babel-python-command "python3.10")
  (setq org-image-actual-width nil)
  (setq org-use-sub-superscripts '{})

  (setq org-latex-compiler "pdflatex"
	org-latex-preview-process-alist '((dvipng :programs ("latex" "dvipng") :description "dvi > png"
						  :message
						  "you need to install the programs: latex and dvipng."
						  :image-input-type "dvi" :image-output-type "png"
						  :latex-compiler
						  ("%l -interaction nonstopmode -output-directory %o %f")
						  :latex-precompiler
						  ("%l -output-directory %o -ini -jobname=%b \"&%L\" mylatexformat.ltx %f")
						  :image-converter
						  ("dvipng --follow -D %D -T tight --depth --height -o %B-%%09d.png %f")
						  :transparent-image-converter
						  ("dvipng --follow -D %D -T tight -bg Transparent --depth --height -o %B-%%09d.png %f"))
					  (dvisvgm :programs ("latex" "dvisvgm") :description "dvi > svg"
						   :message
						   "you need to install the programs: latex and dvisvgm."
						   :image-input-type "dvi" :image-output-type "svg"
						   :latex-compiler
						   ("%l -interaction nonstopmode -output-directory %o %f")
						   :latex-precompiler
						   ("%l -output-directory %o -ini -jobname=%b \"&%L\" mylatexformat.ltx %f")
						   :image-converter
						   ("dvisvgm --page=1- --optimize --clipjoin --relative --no-fonts --exact-bbox -o %B-%%9p.svg %f"))
					  (imagemagick :programs ("pdflatex" "convert") :description
						       "pdf > png" :message
						       "you need to install the programs: latex and imagemagick."
						       :image-input-type "pdf" :image-output-type "png"
						       :latex-compiler
						       ("pdflatex -interaction nonstopmode -output-directory %o %f")
						       :latex-precompiler
						       ("pdflatex -output-directory %o -ini -jobname=%b \"&pdflatex\" mylatexformat.ltx %f")
						       :image-converter
						       ("convert -density %D -trim -antialias %f -quality 100 %B-%%09d.png"))))

  (setq org-latex-classes '(("article" "\\documentclass[11pt]{article}"
			     ("\\usepackage{fontspec}")
			     ("\\setmainfont{Linux Biolinum O}")
			     ("\\setmonofont[Scale=MatchLowercase]{BlexMono Nerd Font Mono}")
			     ;; ("\\usepackage{xeCJK}")
			     ;; ("\\setCJKmainfont{LXGW WenKai}")
			     ;; ("\\setCJKmonofont{LXGW WenKai Mono}")
			     ("\\section{%s}" . "\\section*{%s}")
			     ("\\subsection{%s}" . "\\subsection*{%s}")
			     ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
			     ("\\paragraph{%s}" . "\\paragraph*{%s}")
			     ("\\subparagraph{%s}" . "\\subparagraph*{%s}")
			     )
			    ("report" "\\documentclass[11pt]{report}"
			     ("\\part{%s}" . "\\part*{%s}") ("\\chapter{%s}" . "\\chapter*{%s}")
			     ("\\section{%s}" . "\\section*{%s}")
			     ("\\subsection{%s}" . "\\subsection*{%s}")
			     ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
			    ("book" "\\documentclass[11pt]{book}" ("\\part{%s}" . "\\part*{%s}")
			     ("\\chapter{%s}" . "\\chapter*{%s}")
			     ("\\section{%s}" . "\\section*{%s}")
			     ("\\subsection{%s}" . "\\subsection*{%s}")
			     ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))))

  (setq org-latex-default-packages-alist
	'(("AUTO" "inputenc" t ("pdflatex")) ("T1" "fontenc" t ("pdflatex"))
	  ("" "hyperref" nil)))

  (setq org-latex-preview-precompile nil)

  (setq org-latex-preview-preamble "\\documentclass{article}
[PACKAGES]
\\usepackage{xpatch}
%\\setmainfont{Linux Biolinum O}
%\\setmonofont[Scale=MatchLowercase]{BlexMono Nerd Font Mono}
%\\usepackage{xeCJK}
%\\setCJKmainfont{LXGW WenKai}
%\\setCJKmonofont{LXGW WenKai Mono}
\\usepackage{xcolor}")

  (setq org-latex-packages-alist
	'(("" "longtable" t nil)
	  ("" "wrapfig" t nil)
	  ("" "rotating" t nil)
	  ("normalem" "ulem" t nil)
	  ("" "amsmath" t nil)
	  ("" "mathspec" t ("xelatex"))
	  ("" "amssymb" t nil)
	  ("" "capt-of" t nil)
	  ("" "hyperref" nil nil)
	  ("" "fontspec" t ("xelatex" "lualatex"))
	  ("T1" "fontenc" t ("pdflatex"))))

  (setq org-latex-preview-auto-generate 'live
	org-latex-preview-debounce 1.0
	org-latex-preview-throttle 1.0
	org-latex-preview-numbered nil
	org-startup-with-latex-preview nil)

  ;; dont cache latex preview images
  (setq org-latex-preview-persist nil)
  (setq org-element-cache-persistent nil)
  (setq org-element-use-cache nil))

(use-package prettify-symbols
  :after (org)
  :hook (org-mode . prettify-symbols-mode)
  :config
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
	  ("#+begin_quote:"   . "❝")
	  ("#+end_quote:"     . "❞")
          ("#+BEGIN_QUOTE:"   . "❝")
          ("#+END_QUOTE:"     . "❞")
	  ("#+attr_latex:"    . "🄛")
	  ("#+attr_html:"     . "🄗")
	  ("#+attr_org:"      . "⒪")
	  ("#+ATTR_LATEX:"    . "🄛")
	  ("#+ATTR_HTML:"     . "🄗")
	  ("#+ATTR_ORG:"      . "⒪")
	  )))

(use-package org-remoteimg
  :straight (org-remoteimg :type git :host github :repo "gaoDean/org-remoteimg")
  :config
  (setq org-display-remote-inline-images 'cache))

;; new org mode test

;; (setq org-latex-compiler "xelatex"
;;       org-latex-preview-precompile nil
;;       org-latex-preview-process-alist '((dvisvgm :programs
;; 						 ("xelatex" "dvisvgm")
;; 						 :description "xdv > svg" :message "you need to install the programs: latex and dvisvgm." :image-input-type "xdv" :image-output-type "svg"
;; 						 :latex-compiler
;; 						 ("%l -no-pdf -interaction nonstopmode -output-directory %o %f")
;; 						 :image-converter
;; 						 ("dvisvgm --page=1- --optimize --clipjoin --relative --no-fonts --bbox=preview -o %B-%%9p.svg %f")
;; 						 :post-clean nil)))



(use-package org-variable-pitch
  :after (org)
  :config
  )

(use-package org-faces
  :after (org)
  :init
  (set-face-attribute 'variable-pitch nil :family "EB Garamond" :height 160)
  (set-face-attribute 'org-level-1 nil :weight 'semi-bold :family "EB Garamond" :height 240)
  (set-face-attribute 'org-level-2 nil :family "Linux Biolinum O" :height 210)
  (set-face-attribute 'org-level-3 nil :family "Linux Biolinum O" :height 190)
  (set-face-attribute 'org-level-4 nil :family "Linux Biolinum O" :height 160)
  (set-face-attribute 'fixed-pitch nil :family "FiraCode Nerd Font Mono")
  (set-face-attribute 'org-block nil :family "FiraCode Nerd Font Mono")
  (set-face-attribute 'org-block-begin-line nil :family "FiraCode Nerd Font Mono"))

(use-package org-appear
  :straight t
  :hook (org-mode . org-appear-mode)
  :config
  (setq-default org-hide-emphasis-markers t)
  (setq org-appear-autoemphasis t
        org-appear-autosubmarkers t))

(use-package org-modern
  :straight t
  :hook
  (org-mode . global-org-modern-mode)
  :custom
  ;; Org modern settings
  (org-modern-priority nil)
  (org-modern-list nil)
  (org-modern-checkbox nil)
  (org-modern-todo t)
  (org-modern-keyword nil)
  (org-modern-block nil)
  (org-modern-table nil)
  (org-modern-star '("⚀" "⚁" "⚂" "⚃" "⚄" "⚅"))

  ;; Editor settings
  (org-auto-align-tags nil)
					; (org-tags-column 0)
  (org-catch-invisible-edits 'show-and-error)
  (org-special-ctrl-a/e t)

  :config
  ;; (with-eval-after-load 'org (global-org-modern-mode))
  )

(use-package valign
  :straight t
  :hook
  (org-mode . valign-mode))

;; org roam settings
(use-package org-roam
  :straight t
  :defer t
  :after (org)
  :load-path "~/.emacs.d/site-lisp/org-roam/extensions"
  :init
  (setq org-roam-directory (file-truename "~/Documents/orgs")
	org-roam-database-connector 'sqlite-builtin)  ;; roam 应用的文件夹

  :config
  (org-roam-db-autosync-mode +1)
  )

(use-package org-roam-ui
  :straight t
  :after (org-roam))

(use-package init-dynamic-agenda
  :after (org))

;; whitespace mode 下显示零宽空格
(with-eval-after-load 'whitespace
  (setq whitespace-space-regexp "\\( +\\|\u200b\\)")
  (add-to-list 'whitespace-display-mappings '(space-mark #x200b [?▾])))

(use-package writeroom-mode
  :straight t
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

(setq org-ellipsis "⤵")

;; agenda settings
;; (load "~/.emacs.d/site-lisp/next-spec-day.el")
(setq org-agenda-files '("~/Documents/orgs/agenda.org"))

(use-package org-tempo
  :after (org))

(use-package ob-rust
  :straight t
  :after (org))


;; org-xlatex
(use-package org-xlatex
  :straight t
  :after (org)
  :hook (org-mode . org-xlatex-mode))
(setq org-xlatex-position-indicator t)


;; org-capture
(setq org-default-notes-file "~/Documents/orgs/agenda.org")


;; org-zotxt
(use-package zotxt
  :straight t
  :hook (org-mode . org-zotxt-mode))


;; org-visual-outline
(use-package org-visual-indent
  :straight (org-visual-outline :type git :host github :repo "legalnonsense/org-visual-outline")
  :after (org color)
  :hook (org-mode . org-visual-indent-mode)
  :config
  (setq org-visual-indent-color-indent
      (cl-loop for x from 1 to 8
               with color = nil
               do (setq color (or (face-foreground
                                   (intern
                                    (concat "org-level-"
                                            (number-to-string x))))
                                  (face-foreground 'org-level-1)))
               collect `(,x ,(list
                              :background color
                              :foreground color
                              :height .1)))))
;; (require 'org-visual-indent)
;; (require 'color)
;; (setq org-visual-indent-color-indent
;;       (cl-loop for x from 1 to 8
;;                with color = nil
;;                do (setq color (or (face-foreground
;;                                    (intern
;;                                     (concat "outline-"
;;                                             (number-to-string x))))
;;                                   (face-foreground 'org-level-1)))
;;                collect `(,x ,(list
;;                               :background color
;;                               :foreground color
;;                               :height .1))))
;; If you don’t have all of the org-level-x faces set, this will use
;; org-level-1 as the backup. Or figure out your own way of doing it.
;; None of it matters anyway.


;; org-tidy
(use-package org-tidy
  :straight t
  :hook (org-mode . org-tidy-mode)
  :config
  )


;; org html export theme
(use-package htmlize
  :straight t)

;; Embed inline CSS read from a file.
(defun my-org-inline-css-hook (exporter)
  "Insert custom inline css"
  (when (eq exporter 'html)
    (let* ((dir (ignore-errors (file-name-directory (buffer-file-name))))
           (path (concat dir "style.css"))
           (homestyle (and (or (null dir) (null (file-exists-p path)))
                           (not (null-or-unboundp 'my-org-inline-css-file))))
           (final (if homestyle my-org-inline-css-file path)))
      (if (file-exists-p final)
          (progn
            (setq-local org-html-head-include-default-style nil)
            (setq-local org-html-head (concat
                                       "<style type=\"text/css\">\n"
                                       "<!--/*--><![CDATA[/*><!--*/\n"
                                       (with-temp-buffer
                                         (insert-file-contents final)
                                         (buffer-string))
                                       "/*]]>*/-->\n"
                                       "</style>\n")))))))

(add-hook 'org-export-before-processing-hook 'my-org-inline-css-hook)


;; LaTeX input settings
(use-package auctex
  :straight t)

(provide 'init-org)
