(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(require 'init-benchmarking) ;; Measure startup time; from purcell

(setq read-process-output-max (* 4 1024 1024))

(defconst *is-a-mac* (eq system-type 'darwin))

(setq custom-file (locate-user-emacs-file "custom.el"))
(require 'init-basic)
(require 'init-utils)
(require 'init-const)
(require 'init-site-lisp) ;; Must come before elpa, as it may provide package.el
;; Calls (package-initialize)
(require 'init-elpa)      ;; Machinery for installing required packages
(require 'init-exec-path) ;; Set up $PATH
(require 'init-straight)

(require 'init-osx-keys)
(require 'init-gui-frames)
(require 'init-themes)
(when *is-a-mac*
  (require-package 'osx-location))
(require 'init-meow)
(require 'init-ibuffer)

(require 'init-ivy)
(require 'init-company)
(require 'init-awesomepair)
(require 'init-eglot)
(require 'init-dirvish)
(require 'init-yasnippet)
;; (require 'init-lsp)
(require 'init-treesitter)
(require 'init-org)
