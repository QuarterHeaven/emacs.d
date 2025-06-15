;; -*- lexical-binding: t -*-
;;; vterm
(use-package vterm
  :straight t
  ;; :bind ("C-t" . vterm-toggle)
  :config
  (defvar vterm-compile-buffer nil)
  (defun vterm-compile ()
    "Compile the program including the current buffer in `vterm'."
    (interactive)
    (let* ((command (eval compile-command))
           (w (vterm-toggle--get-window)))
      (setq compile-command (compilation-read-command command))
      (let ((vterm-toggle-use-dedicated-buffer t)
            (vterm-toggle--vterm-dedicated-buffer (if w (vterm-toggle-hide)
                                                    vterm-compile-buffer)))
        (with-current-buffer (vterm-toggle-cd)
          (setq vterm-compile-buffer (current-buffer))
          (rename-buffer "*vterm compilation*")
          (compilation-shell-minor-mode 1)
          (vterm-send-M-w)
          (vterm-send-string compile-command t)
          (vterm-send-return)))))
  (defvar vterm-buffer-name  "*vterm*")
  (setq vterm-toggle-fullscreen-p nil)
  (add-to-list 'display-buffer-alist
               '((lambda (buffer-or-name _)
                   (let ((buffer (get-buffer buffer-or-name)))
                     (with-current-buffer buffer
                       (or (equal major-mode 'vterm-mode)
                           (string-prefix-p vterm-buffer-name (buffer-name buffer))))))
                 (display-buffer-reuse-window display-buffer-at-bottom)
                 ;;(display-buffer-reuse-window display-buffer-in-direction)
                 ;;display-buffer-in-direction/direction/dedicated is added in emacs27
                 ;;(direction . bottom)
                 ;;(dedicated . t) ;dedicated is supported in emacs27
                 (reusable-frames . visible)
                 (window-height . 0.3))))

(use-package vterm-toggle
  :disabled
  :straight t
  :after (vterm)
  :config
  (setq vterm-toggle-hide-method 'delete-window))

;;; eat + eshell
(use-package eat
  :straight (:type git
		   :host codeberg
		   :repo "akib/emacs-eat"
		   :files ("*.el" ("term" "term/*.el") "*.texi"
			   "*.ti" ("terminfo/e" "terminfo/e/*")
			   ("terminfo/65" "terminfo/65/*")
			   ("integration" "integration/*")
			   (:exclude ".dir-locals.el" "*-tests.el"))))

(use-package eshell
  :hook (eshell-load . eat-eshell-mode)
  :bind ("C-t" . eshell))

(provide 'init-vterm)
