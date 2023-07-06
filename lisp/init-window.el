(require 'ace-window)
(require 'eyebrowse)

(global-set-key (kbd "C-x o") 'ace-window)
(eyebrowse-mode t)
(eyebrowse-setup-opinionated-keys)

;; (desktop-save-mode 1)
;; (push '(company-posframe-mode . nil)
;;       desktop-minor-mode-table)
;; (run-with-idle-timer 30 t (lambda () (desktop-save "~/.emacs.d")))

(provide 'init-window)
