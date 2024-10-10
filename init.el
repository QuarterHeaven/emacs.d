(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; (toggle-debug-on-error)

(let (
      ;; 加载的时候临时增大`gc-cons-threshold'以加速启动速度。
      (gc-cons-threshold most-positive-fixnum)
      ;; 清空避免加载远程文件的时候分析文件。
      (file-name-handler-alist nil))
  ;;  ;; 下面才写你的其它配置

  (setq
   ;; 默认用最简单的模式
   initial-major-mode 'fundamental-mode
   ;; 不要自动启用package
   package-enable-at-startup nil
   package--init-file-ensured t)

  (setq read-process-output-max (* 4 1024 1024))

  (defconst *is-a-mac* (eq system-type 'darwin))

					;(with-temp-message ""
  (setq custom-file (locate-user-emacs-file "custom.el"))
  (load custom-file)

  (require 'init-utils)
  (require 'init-const)
  (require 'init-basic)
  (require 'init-site-lisp) ;; Must come before elpa, as it may provide package.el
  ;; Calls (package-initialize)
  ;; (require 'init-elpa)      ;; Machinery for installing required packages
  (require 'init-straight)
  (straight-use-package 'project)

  (require 'init-exec-path)   ;; Set up $PATH
  (require 'bind-key)
  (require 'init-autosave)
  (require 'init-outli)
  (require 'init-latex)
  (require 'init-org)

  (require 'init-osx-keys)
  (require 'init-gui-frames)
  (require 'init-pixel-scroll)
  (require 'init-window)
  (require 'init-popper)
  (require 'init-fonts)
  (require 'init-themes)
  (require 'init-meow)
  (require 'init-dogears)
  (require 'init-ibuffer)
  (require 'init-dashboard)
  (require 'init-yasnippet)
  (require 'init-completion)
  (require 'init-lspmode)
  (require 'init-magit)
  (require 'init-vertico)
  (require 'init-blink)
  (require 'init-dirvish)
  (require 'init-treesitter)
  (require 'init-pdftools)
  ;; (require 'init-dynamic-agenda)
  (require 'init-rime)
  (require 'init-vterm)
  (require 'init-terminal-here)
  (require 'init-rg)
  (require 'init-projectile)
  (require 'init-treemacs)
  (require 'init-keyfreq)
  (require 'init-autocompile)
  (require 'init-gpg)
  (require 'init-telega)

  (require 'init-languages)
  (require 'init-clojure)
  (require 'init-ejc-sql)
  (require 'init-web)
  (require 'init-wolfram)
  (require 'init-typst)

  (require 'init-cloud)
  (require 'init-esup)
  (require 'init-emms)
  (require 'init-tokenizer)
  (require 'init-fileinfo)
  (require 'init-ai)
  (require 'init-translate)
  (require 'init-dictionary)

  (require 'init-secrets)
  (require 'init-keybinding)
  )
