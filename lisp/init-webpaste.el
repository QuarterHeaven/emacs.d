(require 'webpaste)

(use-package webpaste
  :bind (("C-c C-p C-b" . webpaste-paste-buffer)
         ("C-c C-p C-r" . webpaste-paste-region)
         ("C-c C-p C-p" . webpaste-paste-buffer-or-region))
  :config
  (progn
    (setq webpaste-provider-priority '("paste.mozilla.org" "paste.ubuntu.com"))))

(provide 'init-webpaste)
