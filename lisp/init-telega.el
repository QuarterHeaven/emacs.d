(use-package telega
  :straight t
  :defer t
  :commands (telega)
  :init
  (setq telega-chat-folder-format nil)
  :hook
  (telega-root-mode . (lambda() (setq bidi-display-reordering t)))
  (telega-chat-mode . (lambda() (setq bidi-display-reordering t)))
  ;; 固定 telega 窗口在右侧
  ( telega-mode . (lambda ()
		    (display-buffer (current-buffer) '((display-buffer-in-side-window)))))

  :config
  (setq telega-use-docker nil
	telega-root-default-view-function 'telega-view-folders
	telega-root-keep-cursor 'track
	telega-root-fill-column 34
	telega-chat-show-avatars t
	telega-user-show-avatars t
	telega-root-show-avatars t
        telega-filters-custom nil
	telega-emoji-use-images nil
        telega-filter-custom-show-folders nil
	telega-symbol-folder "  "
	telega-translate-to-language-by-default "zh"
	telega-chat-input-markups '("org")
	telega-chat-prompt-format "> "
	telega-completing-read-function completing-read-function
	telega-proxies (list '(:server "127.0.0.1" :port 1081 :enable t :type (:@type "proxyTypeSocks5"))))

  (add-to-list 'display-buffer-alist
	'(("\\*Telega Root\\*" . ((display-buffer-in-side-window)
				  (window-width . 0.35)
				  (side . left)
				  (slot . 0)
				  (dedicated . nil)
				  (window-parameters . ((no-other-window . t)))))))

  (setq telega-url-shorten-regexps
             ;; telega-url-shorten
             (list `(too-long-link
                     :regexp "^\\(https?://\\)\\(.\\{55\\}\\).*?$"
                     :symbol ""
                     :replace "\\1\\2...")))
  )

(provide 'init-telega)
