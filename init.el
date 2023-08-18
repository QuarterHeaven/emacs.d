(add-to-list 'load-path "~/.emacs.d/site-lisp/benchmark-init-el/")
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(require 'init-benchmarking) ;; Measure startup time; from purcell

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

					;(with-temp-message ""
(setq custom-file (locate-user-emacs-file "custom.el"))
(load custom-file)

(require 'init-utils)
(require 'init-basic)
(require 'init-const)
(require 'init-site-lisp) ;; Must come before elpa, as it may provide package.el
;; ;; Calls (package-initialize)
;; 					;  (require 'init-elpa)      ;; Machinery for installing required packages
(add-to-list 'load-path "~/.emacs.d/site-lisp/org-mode/lisp")
(require 'init-exec-path) ;; Set up $PATH
;; (require 'cache-path-from-shell)
(require 'bind-key)
(require 'init-autosave)

(require 'init-osx-keys)
(require 'init-gui-frames)
(require 'init-window)
(require 'init-holo-layer)
(require 'init-themes)
(require 'init-fonts)
(when *is-a-mac*
  (require 'osx-location))
(require 'init-meow)
(require 'init-ibuffer)
(require 'init-dashboard)
(require 'init-yasnippet)
(require 'init-lspbridge)
(require 'init-magit)
;; (require 'init-ivy)
(require 'init-vertico)
(require 'init-fingertip)
(require 'init-dirvish)
(require 'init-treesitter)
(require 'init-org)
(require 'init-rime)
(require 'init-vterm)
(require 'init-rg)
(require 'init-projectile)
(require 'init-treemacs)
(require 'init-keyfreq)
(require 'init-autocompile)
(require 'init-gpg)
(require 'init-eaf)

(require 'init-languages)
(require 'init-clojure)
(require 'init-web)
(require 'init-deno)
;)
