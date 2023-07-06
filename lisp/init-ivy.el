(require 'ivy)
(require 'counsel)
(require 'swiper)
(require 'ivy-dired-history)
(require 'counsel-projectile)
(require 'smex)
(require 'all-the-icons-ivy-rich)
(require 'which-key)
(require 'which-key-posframe)

(use-package ivy
  ;; :defer t
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (with-eval-after-load 'counsel
    (setq ivy-initial-inputs-alist nil))
  (counsel-mode 1)

  (setq counsel-find-file-at-point t
	counsel-preselect-current-file t
	counsel-yank-pop-separator "\n────────\n")
  (add-hook 'counsel-grep-post-action-hook #'recenter)

  (setq ivy-height 12
	ivy-use-selectable-prompt t
	ivy-use-virtual-buffers t    ; Enable bookmarks and recentf
	ivy-fixed-height-minibuffer t
	ivy-count-format "(%d/%d) "
	ivy-ignore-buffers '("\\` " "\\`\\*tramp/" "\\`\\*xref" "\\`\\*helpful "
                             "\\`\\*.+-posframe-buffer\\*" "\\` ?\\*company-.+\\*")
	ivy-on-del-error-function #'ignore
	ivy-initial-inputs-alist nil)

  (setq ivy-re-builders-alist '((t . ivy--regex-ignore-order))))

(require 'ivy-rich)
(all-the-icons-ivy-rich-mode 1)
(ivy-rich-mode 1)
(setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)

(defun ivy-rich-switch-buffer-icon (candidate)
  (with-current-buffer
      (get-buffer candidate)
    (let ((icon (all-the-icons-icon-for-mode major-mode)))
      (if (symbolp icon)
          (all-the-icons-icon-for-mode 'fundamental-mode)
        icon))))

;; Use Ivy to open recent directories
(use-package ivy-dired-history
  :demand t
  :after dired
  :defines (savehist-additional-variables desktop-globals-to-save)
  :bind (:map dired-mode-map
         ("," . dired))
  :init
  (with-eval-after-load 'savehist
    (add-to-list 'savehist-additional-variables 'ivy-dired-history-variable))
  (with-eval-after-load 'desktop
    (add-to-list 'desktop-globals-to-save 'ivy-dired-history-variable)))

(use-package counsel-projectile
  :hook (counsel-mode . counsel-projectile-mode)
  :init
  (setq counsel-projectile-grep-initial-input '(ivy-thing-at-point))
  (when (executable-find "ugrep")
    (setq counsel-projectile-grep-base-command "ugrep --color=never -rnEI %s")))

;; (require-package 'ivy-posframe)
;; display at `ivy-posframe-style'
;; (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display)))
;; (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-center)))
;; (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-window-center)))
;; (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-bottom-left)))
;; (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-window-bottom-left)))
;; (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-top-center)))
;; (ivy-posframe-mode 1)
;; (add-hook 'ivy-mode-hook
;;           (lambda ()
;; 	    (if (equal major-mode 'eaf-mode)
;;                 (ivy-posframe-mode -1)
;;               (ivy-posframe-mode 1))
;;             ))

(setq ivy-height 15                 ; Use bigger minibuffer height for child frame
      ivy-posframe-border-width 3
      ivy-posframe-parameters '((left-fringe . 8)
                                (right-fringe . 8)))

(use-package smex)

(which-key-mode)
(setq which-key-posframe-poshandler 'posframe-poshandler-frame-center)
(which-key-posframe-mode)

(provide 'init-ivy)
