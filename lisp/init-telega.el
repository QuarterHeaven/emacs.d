(use-package telega
  :straight t
  :defer t
  :commands (telega)
  :hook
  (telega-load . telega-appindicator-mode)
  :init
  (setq ;; telega-chat-folders-insexp nil
   telega-docker-volumes '("/run/current-system/sw/share/X11/xkb")
   telega-debug t)
  (add-hook 'telega-load-hook
	    #'(lambda ()
		(set-face-attribute 'telega-msg-heading nil
				    :background nil
                                    :underline 't
                                    :height 1.2
                                    )
		(set-face-attribute 'telega-msg-inline-forward nil
                                    ;; :background "light gray"
                                    :underline nil
                                    :height 0.84)
		(set-face-attribute 'telega-msg-inline-reply nil
                                    ;; :background "light gray"
                                    :underline nil
                                    :height 0.84)
		))
  :hook
  (telega-root-mode . (lambda() (setq bidi-display-reordering t)))
  (telega-chat-mode . (lambda() (setq bidi-display-reordering t)))
  (telega-chat-mode . (lambda()
			(visual-line-mode 1)
			(visual-fill-column-mode 1)
			(setq-local visual-fill-column-extra-text-width '(0 . 3))))
  ;; 固定 telega 窗口在右侧
  ;; (telega-chat-mode . (lambda ()
  ;; 			(display-buffer (current-buffer) '((display-buffer-in-side-window)))))
  ;; (telega-root-mode . (lambda ()
  ;; 			(display-buffer (current-buffer) '((display-buffer-in-side-window)))))

  :config
  (setq telega-root-default-view-function 'telega-view-folders
	telega-root-keep-cursor 'track
	;; telega-root-fill-column 70
	telega-chat-fill-column 90
	telega-chat-show-avatars t
	telega-user-show-avatars t
	telega-root-show-avatars t
	telega-chat-show-deleted-messages-for t
        telega-filters-custom nil
	telega-emoji-use-images nil
        telega-filter-custom-show-folders nil
	telega-symbol-folder "  "
	telega-translate-to-language-by-default "zh"
	telega-chat-input-markups '("org")
	telega-chat-prompt-format "> "
	telega-completing-read-function completing-read-function
	telega-proxies (list '(:server "127.0.0.1" :port 1081 :enable t :type (:@type "proxyTypeSocks5")))
	telega-avatar-workaround-gaps-for '(return t)
	)

  (if sys/macp (setq telega-use-docker nil)
    (setq telega-use-docker t)
    )

  ;; src: https://emacs-china.org/t/telega/25759/16 by #1ab, double checkmark
  (setq telega-symbols-emojify (assq-delete-all 'checkmark telega-symbols-emojify))
  (setq telega-symbols-emojify (assq-delete-all 'heavy-checkmark telega-symbols-emojify))
  (setq telega-symbol-checkmark (nerd-icons-codicon "nf-cod-check"))
  (setq telega-symbol-heavy-checkmark (nerd-icons-codicon "nf-cod-check_all"))

  ;; (setq telega-symbols-emojify (assq-delete-all 'reply telega-symbols-emojify))
  ;; (setq telega-symbols-emojify (assq-delete-all 'reply-quote telega-symbols-emojify))
  ;; (setq telega-symbols-emojify (assq-delete-all 'forward telega-symbols-emojify))
  ;; (setq telega-symbol-reply "➦")
  ;; (setq telega-symbol-reply-quote "➦⑆")
  ;; (setq telega-symbol-forward "➲")

  ;; (add-to-list 'display-buffer-alist
  ;; 	'(("\\*Telega Root\\*" . ((display-buffer-in-side-window)
  ;; 				  (window-width . 0.35)
  ;; 				  (side . left)
  ;; 				  (slot . 0)
  ;; 				  (dedicated . nil)
  ;; 				  (window-parameters . ((no-other-window . t)))))))

  (setq telega-url-shorten-regexps
        ;; telega-url-shorten
        (list `(too-long-link
                :regexp "^\\(https?://\\)\\(.\\{55\\}\\).*?$"
                :symbol ""
                :replace "\\1\\2...")))
  (advice-add #'telega-msg-goto :after #'(lambda () (recenter)
					   (pulse-momentary-highlight-one-line (point))))

  (setq telega-msg-heading-with-date-and-status nil)
  (require 'telega-mnz)
  (add-hook 'telega-load-hook 'global-telega-mnz-mode))

(provide 'init-telega)
