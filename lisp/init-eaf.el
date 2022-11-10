(use-package eaf
  :load-path "~/.emacs.d/site-lisp/emacs-application-framework"
  :custom
					; See https://github.com/emacs-eaf/emacs-application-framework/wiki/Customization
  (eaf-browser-continue-where-left-off t)
  (eaf-browser-enable-adblocker t)
  (browse-url-browser-function 'eaf-open-browser)
  :config
  (defalias 'browse-web #'eaf-open-browser)
  (require 'eaf-file-sender)
  (require 'eaf-camera)
  (require 'eaf-rss-reader)
  (require 'eaf-terminal)
  (require 'eaf-image-viewer)
  (require 'eaf-pdf-viewer)
  (require 'eaf-browser)
  (require 'eaf-markdown-previewer)
  (require 'eaf-file-browser)
  (require 'eaf-file-manager)
  (require 'eaf-mindmap)
  (require 'eaf-video-player)
  (require 'eaf-org-previewer)
  (require 'eaf-airshare)
  (require 'eaf-system-monitor) ;; unbind, see more in the Wiki
  (setq eaf-webengine-default-zoom (if (> (frame-pixel-width) 3000) 2 1))
  (setq eaf-browser-aria2-proxy-host "127.0.0.1")
  (setq eaf-browser-aria2-proxy-port "1081")
  (setq eaf-browser-enable-adblocker t)
  (setq eaf-browser-enable-autofill t)
  (setq eaf-terminal-font-size 18)
  (setq eaf-webengine-font-family "LXGW WenKai")
  (setq eaf-webengine-fixed-font-family "LXGW WenKai Mono")
  (setq eaf-webengine-serif-font-family "TsangerJinKai03-6763")
  (setq eaf-webengine-font-size 18)
  (setq eaf-webengine-fixed-font-size 18)
  (setq eaf-terminal-font-family "FiraCode Nerd Font")
  (setq eaf-file-manager-show-hidden-file nil))

(provide 'init-eaf)
