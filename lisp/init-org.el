(use-package visual-fill-column
  :straight t)

(use-package org
  :defer t
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
   (org-mode . org-toggle-pretty-entities)
   (org-mode . org-cdlatex-mode))
  :bind ("C-c <backspace>" . org-mark-ring-goto)
  :init (require 'tex-mode)
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (C . t)
     (java . t)
     (js . t)
     (python . t)
     (shell . t)
     (latex . t)
     (rust . t)
     ;; (yaml . t)
     ))
  (when (executable-find "jupyter")
    (add-to-list 'org-babel-load-languages '(jupyter . t) t))
  (setq word-wrap-by-category t
	org-babel-python-command "python3.10"
	org-image-actual-width nil)

  (defun my/org-raise-scripts-no-braces (_)
    (when (and (eq (char-after (match-beginning 3)) ?{)
               (eq (char-before (match-end 3)) ?}))
      (remove-text-properties (match-beginning 3) (1+ (match-beginning 3))
			      (list 'invisible nil))
      (remove-text-properties (1- (match-end 3)) (match-end 3)
			      (list 'invisible nil))))

  (advice-add 'org-raise-scripts :after #'my/org-raise-scripts-no-braces)

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
	org-latex-preview-debounce 2.0
	org-latex-preview-throttle 1.0
	org-latex-preview-numbered nil
	;; org-startup-with-latex-preview t
	)
  (setq org-latex-preview-options '(:foreground auto
						:background "Transparent" :scale 1.0
						:matchers
						("begin" "$1" "$" "$$" "\\(" "\\[") :zoom 1.1))

  (setq org-highlight-latex-and-related '(entities native latex))

  ;; dont cache latex preview images
  (setq org-latex-preview-persist nil)
  (setq org-element-cache-persistent nil)
  (setq org-element-use-cache nil))

(use-package prettify-symbols
  :hook
  (org-mode . prettify-symbols-mode)
  (org-mode . (lambda () (setq prettify-symbols-alist
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
				 ("#+begin_quote"   . "❝")
				 ("#+end_quote"     . "❞")
				 ("#+BEGIN_QUOTE"   . "❝")
				 ("#+END_QUOTE"     . "❞")
				 ("#+attr_latex"    . "🄛")
				 ("#+attr_html"     . "🄗")
				 ("#+attr_org"      . "⒪")
				 ("#+ATTR_LATEX"    . "🄛")
				 ("#+ATTR_HTML"     . "🄗")
				 ("#+ATTR_ORG"      . "⒪")
				 ))))
  :init
  (setq org-startup-with-inline-images t))

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
  ;; (set-face-attribute 'variable-pitch nil :family "EB Garamond" :height 160)
  (if sys/WSL
      (progn
	(set-face-attribute 'variable-pitch nil :family "Alegreya" :height 200)
	(set-face-attribute 'org-level-1 nil :weight 'semi-bold :family "EB Garamond" :height 300)
	(set-face-attribute 'org-level-2 nil :family "Linux Biolinum O" :height 262)
	(set-face-attribute 'org-level-3 nil :family "Linux Biolinum O" :height 237)
	(set-face-attribute 'org-level-4 nil :family "Linux Biolinum O" :height 200))
    (progn
      (set-face-attribute 'variable-pitch nil :family "Alegreya" :height 160)
      (set-face-attribute 'org-level-1 nil :weight 'semi-bold :family "EB Garamond" :height 240)
      (set-face-attribute 'org-level-2 nil :family "Linux Biolinum O" :height 210)
      (set-face-attribute 'org-level-3 nil :family "Linux Biolinum O" :height 190)
      (set-face-attribute 'org-level-4 nil :family "Linux Biolinum O" :height 160)))
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
  (setq line-spacing 0.2)
  )

(use-package org-margin
  :disabled
  :straight (:host github :repo "rougier/org-margin")
  :init
  (org-margin-mode 1)
  :config
  (setq org-margin-markers (("\\(#\\+begin_src\\)" .
			     #(" " 0 2 (face (font-lock-comment-face bold))))
			    ("\\(#\\+BEGIN_SRC\\)" .
			     #(" " 0 2 (face (font-lock-comment-face bold))))
			    ("\\(#\\+begin_quote\\)" .
			     #(" " 0 2 (face (font-lock-comment-face bold))))
			    ("\\(#\\+BEGIN_QUOTE\\)" .
			     #(" " 0 2 (face (font-lock-comment-face bold)))))))

(use-package valign
  :straight t
  :hook
  (org-mode . valign-mode))


;; org roam settings
(use-package s
  :straight t)

(use-package org-roam
  :straight t
  ;; :after (org)
  :defer t
  :load-path "~/.emacs.d/site-lisp/org-roam-db-last-update-time.el"
  :bind
  (("C-c r f" . 'org-roam-node-find)
   ("C-c r a" . 'org-roam-tag-add)
   ("C-c r d" . 'org-roam-tag-remove)
   ("C-c r i" . 'org-roam-node-insert)
   ("C-c r t" . 'org-roam-buffer-toggle))
  :init
  (setq org-roam-directory (file-truename "~/Documents/orgs")
	org-roam-database-connector 'sqlite-builtin)  ;; roam 应用的文件夹
  ;; (require 'org-roam-backlink-collections)

  ;; :hook
  ;; (org-mode . org-roam-backlink-collections-mode)

  :config
  (org-roam-db-autosync-mode +1)
  ;; If you're using a vertical completion framework, you might want a more informative completion interface
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:20}" 'face 'org-tag))))

(use-package org-roam-ui
  :straight t
  :after (org-roam))

;; (use-package init-dynamic-agenda
;;   :after (org))

;; whitespace mode 下显示零宽空格
(with-eval-after-load 'whitespace
  (setq whitespace-space-regexp "\\( +\\|\u200b\\)")
  (add-to-list 'whitespace-display-mappings '(space-mark #x200b [?▾])))

(use-package writeroom-mode
  :straight t
  :hook
  ;; (after-init . global-writeroom-mode)
  ((c-ts-mode c++-ts-mode rust-ts-mode python-ts-mode haskell-ts-mode clojure-ts-mode typst-ts-mode) . writeroom-mode)
  ((prog-mode conf-mode yaml-mode shell-mode eshell-mode) . writeroom-mode)
  (org-mode . writeroom-mode)
  (dashboard-mode . writeroom-mode)
  (org-mode . variable-pitch-mode)
  (org-mode . visual-line-mode)
  (visual-line-mode . visual-fill-column-mode)

  :config
  (setq split-width-threshold 120

        writeroom-width 128
        writeroom-bottom-divider-width 0
        writeroom-fringes-outside-margins nil
        writeroom-fullscreen-effect nil
        writeroom-major-modes '(text-mode prog-mode conf-mode special-mode Info-mode dired-mode)
        writeroom-major-modes-exceptions '(process-menu-mode proced-mode)
        writeroom-maximize-window nil
        writeroom-mode-line t
        writeroom-mode-line-toggle-position 'mode-line-format))

(setq org-ellipsis "⤵")

;; agenda settings
;; (load "~/.emacs.d/site-lisp/next-spec-day.el")
(setq org-agenda-files '("~/Documents/orgs/agendas/"))

;; (use-package org-super-agenda
;;   :straight t
;;   :after (org)
;;   :hook (org-agenda . org-super-agenda-mode)
;;   :init
;;   (let ((org-super-agenda-groups
;;        '(;; Each group has an implicit boolean OR operator between its selectors.
;;          (:name "Today"  ; Optionally specify section name
;;                 :time-grid t  ; Items that appear on the time grid
;;                 :todo "TODAY")  ; Items that have this TODO keyword
;;          (:name "Important"
;;                 ;; Single arguments given alone
;;                 :tag "bills"
;;                 :priority "A")
;;          (:priority<= "B"
;;                       ;; Show this section after "Today" and "Important", because
;;                       ;; their order is unspecified, defaulting to 0. Sections
;;                       ;; are displayed lowest-number-first.
;;                       :order 1)
;;          ;; After the last group, the agenda will display items that didn't
;;          ;; match any of these groups, with the default order position of 99
;;          )))
;;   (org-agenda nil "a")))

(use-package org-tempo
  :after (org))

(use-package ob-rust
  :straight t
  :after (org))


;; org-xlatex
(if sys/macp
    (use-package org-xlatex
      :straight t
      :after (org)
      :hook (org-mode . org-xlatex-mode)
      :config
      (setq org-xlatex-position-indicator t)))



;; org-capture
(setq org-default-notes-file "~/Documents/orgs/agenda.org")

(use-package org
  :defer t
  :bind
  ("C-c c" . 'org-capture)
  :config
  (defun my/org-capture-maybe-create-id ()
    (when (org-capture-get :create-id)
      (org-id-get-create)))
  (add-hook 'org-capture-prepare-finalize-hook #'my/org-capture-maybe-create-id)

  (setq org-capture-templates
	'(("t" "Tasks")
	  ("tc" "Class Task" entry
	   (file+headline "~/Documents/orgs/agendas/Class.org" "Class")
	   "* TODO %^{任务名}\n%iCaptured On: %U\nSCHEDULED:%^T\nDEADLINE:%^T" :empty-lines-before 1 :create-id t)
	  ("tt" "Normal Task" entry
	   (file+headline "~/Documents/orgs/agendas/Tasks.org" "Tasks")
	   "* TODO %^{任务名}\n%iCaptured On: %U" :empty-lines-before 1 :create-id t)))
  )


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
           (path (concat dir "style2.css"))
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


;; xeft for note searching
(use-package xeft
  :straight t
  :config
  (setq xeft-recursive t))


;; org-download for insert images under linux (on mac use org-insert-image defined at init-utils)
(use-package org-download
  :straight t
  :hook
  (org-mode . org-download-enable)
  :config
  (setq-default org-download-image-dir "~/Documents/orgs/img")
  (setq org-download-timestamp t
	org-download-backend "curl"))


(use-package djvu
  :straight t)

(use-package org-noter
  :after (djvu)
  :straight t
  :config
  ;; (require 'org-noter-pdftools)
  )

(use-package org-pdftools
  :disabled
  :straight t
  :hook (org-mode . org-pdftools-setup-link))

(use-package org-noter-pdftools
  :disabled
  :straight t
  :after org-noter
  :bind
  ()
  :config
  ;; Add a function to ensure precise note is inserted
  (defun org-noter-pdftools-insert-precise-note (&optional toggle-no-questions)
    (interactive "P")
    (org-noter--with-valid-session
     (let ((org-noter-insert-note-no-questions (if toggle-no-questions
                                                   (not org-noter-insert-note-no-questions)
                                                 org-noter-insert-note-no-questions))
           (org-pdftools-use-isearch-link t)
           (org-pdftools-use-freepointer-annot t))
       (org-noter-insert-note (org-noter--get-precise-info)))))

  ;; fix https://github.com/weirdNox/org-noter/pull/93/commits/f8349ae7575e599f375de1be6be2d0d5de4e6cbf
  (defun org-noter-set-start-location (&optional arg)
    "When opening a session with this document, go to the current location.
With a prefix ARG, remove start location."
    (interactive "P")
    (org-noter--with-valid-session
     (let ((inhibit-read-only t)
           (ast (org-noter--parse-root))
           (location (org-noter--doc-approx-location (when (called-interactively-p 'any) 'interactive))))
       (with-current-buffer (org-noter--session-notes-buffer session)
         (org-with-wide-buffer
          (goto-char (org-element-property :begin ast))
          (if arg
              (org-entry-delete nil org-noter-property-note-location)
            (org-entry-put nil org-noter-property-note-location
                           (org-noter--pretty-print-location location))))))))
  (with-eval-after-load 'pdf-annot
    (add-hook 'pdf-annot-activate-handler-functions #'org-noter-pdftools-jump-to-note)))


;; LOGBOOK settings
(use-package org
  :config
  (setq org-todo-keywords '((sequence "TODO(t)" "|" "DONE(d!)")
			    (sequence "REPORT(r)" "BUG(b@/!)" "KNOWNCAUSE(k)" "|" "FIXED(f)")
			    (sequence "|" "CANCELED(c@)")))
  (setq org-clock-into-drawer t))

(provide 'init-org)
