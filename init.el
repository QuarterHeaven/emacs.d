(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(require 'init-benchmarking) ;; Measure startup time; from purcell

(add-to-list 'load-path "/Users/takaobsid/.emacs.d/site-lisp/benchmark-init-el/")
(let (
      ;; 加载的时候临时增大`gc-cons-threshold'以加速启动速度。
      (gc-cons-threshold most-positive-fixnum)
      ;; 清空避免加载远程文件的时候分析文件。
      (file-name-handler-alist nil))
  (require 'benchmark-init-modes)
  (require 'benchmark-init)
  (benchmark-init/activate)

    ;; 下面才写你的其它配置
)

(setq
 ;; 默认用最简单的模式
 initial-major-mode 'fundamental-mode
 ;; 不要自动启用package
 package-enable-at-startup nil
 package--init-file-ensured t)

(setq read-process-output-max (* 4 1024 1024))

(defconst *is-a-mac* (eq system-type 'darwin))

(with-temp-message ""
  (setq custom-file (locate-user-emacs-file "custom.el"))
  (require 'init-basic)
  (require 'init-utils)
  (require 'init-const)
  (require 'init-site-lisp) ;; Must come before elpa, as it may provide package.el
  ;; Calls (package-initialize)
  (require 'init-elpa)      ;; Machinery for installing required packages
  (require 'init-exec-path) ;; Set up $PATH
  ;; (require 'init-straight)
  (require 'bind-key)
  (require 'init-autosave)

  (require 'init-osx-keys)
  (require 'init-gui-frames)
  (require 'init-window)
  (require 'init-fonts)
  (require 'init-themes)
  (when *is-a-mac*
    (require-package 'osx-location))
  (require 'init-meow)
  (require 'init-ibuffer)
  (require 'init-dashboard)

  (run-with-idle-timer
   1 nil
   #'(lambda ()
       (require 'init-ivy)
       ;; (require 'init-company)
       (require 'init-grammatical-edit)
       (require 'init-eglot)
       (require 'init-dirvish)
       (require 'init-yasnippet)
       (require 'init-treesitter)
       (require 'init-org)
       (require 'init-rime)
       (require 'init-magit)
       (require 'init-aweshell)
       (require 'init-rg)
       (require 'init-projectile)
       (require 'init-treemacs)
       (require 'init-keyfreq)
       (require 'init-autocompile)
       (require 'init-lspbridge)
       (require 'init-gpg)
       (require 'init-eaf)
       (require 'init-deno)

       (require 'init-clojure))))
