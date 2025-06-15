;; -*- lexical-binding: t -*-
;;; ace-window to jump to other window
(use-package ace-window
  :disabled
  :straight t
  :bind
  ("C-x o" . ace-window))

(use-package window-numbering
  :straight t
  :hook (after-init . window-numbering-mode)
  )

(use-package auto-dim-other-buffers
  :straight t
  :hook ((after-init . auto-dim-other-buffers-mode)
         (auto-dim-other-buffers-mode . +auto-dim-other-buffers-auto-set-face))
  :config
  (setq auto-dim-other-buffers-dim-on-focus-out nil
        auto-dim-other-buffers-dim-on-switch-to-minibuffer nil)

  (defadvice! +auto-dim-other-buffers-auto-set-face (&rest _)
    :after #'enable-theme
    (set-face-background 'auto-dim-other-buffers-face (face-background 'mode-line)))
  )

;; (eyebrowse-mode t)
;; (eyebrowse-setup-opinionated-keys)

;; (desktop-save-mode 1)
;; (push '(company-posframe-mode . nil)
;;       desktop-minor-mode-table)
;; (run-with-idle-timer 30 t (lambda () (desktop-save "~/.emacs.d")))

;;; transpose-frame
(use-package transpose-frame
  :straight t
  :hook (meow-insert-exit . sis-set-english)
  :config
  (setq sis-external-ism "macism"
	sis-english-source "com.apple.keylayout.ABC"
	sis-inline-tighten-head-rule nil
	sis-default-cursor-color "#cf7fa7"
	sis-other-cursor-color "orange"
	sis-global-cursor-color-mode t
	sis-global-respect-mode t
	sis-global-context-mode t)
  (add-to-list 'sis-context-hooks 'meow-insert-enter-hook)
  (add-to-list 'sis-context-detectors
               (lambda (&rest _)
                 (when (and meow-insert-mode
                            (or (derived-mode-p 'org-mode
                                                'gfm-mode
                                                'telega-chat-mode)
                                (string-match-p "*new toot*" (buffer-name))))
                   'other)))

  (defun +meow-focus-change-function ()
    (if (frame-focus-state)
        (sis-set-english)
      (meow-insert-exit)))

  (add-function :after after-focus-change-function '+meow-focus-change-function)
  (if sys/macp
      (sis-ism-lazyman-config
       "com.apple.keylayout.ABC"
       "im.rime.inputmethod.Squirrel.Hans"))
  
  )
;;; zoom
(use-package zoom
  :straight t
  ;; :hook (after-init . zoom-mode)
  :bind
  ("C-=" . zoom)
  :config
  (setq zoom-size '(0.618 . 0.618)
	zoom-ignored-major-modes '(dired-mode
				   vterm-mode
				   dape-repl-mode
				   dape-info-breakpoint-mode
				   dape-info-scope-mode
				   dape-info-stack-mode)
	zoom-ignored-buffer-names '("*scratch*")
	zoom-ignored-buffer-name-regexps '("^\\*calc" "\\*vterm" "\\*doom:scratch*")
	zoom-ignored-buffer-name-prefixes '("magit-" "*dape")))
  

(provide 'init-window)
