;;; init-notify.el ---                               -*- lexical-binding: t; -*-

;; Copyright (C) 2025  TakaObsid

;; Author: TakaObsid <takaobsid@Leyline>
;; Keywords:

(use-package knockknock
  :straight (:host github :repo "konrad1977/knockknock")
  :custom
  ;; Duration
  (knockknock-default-duration 5)

  ;; Colors (nil = use theme defaults)
  (knockknock-background-color nil)  ; Use theme background
  (knockknock-foreground-color nil)  ; Use theme foreground
  (knockknock-border-color "orange")

  (knockknock-border-width 2)
  (knockknock-left-fringe 10)
  (knockknock-right-fringe 10)

  (knockknock-use-icons t)                ; Enable/disable icons
  (knockknock-default-icon "fa-bell")     ; Default icon

  (knockknock-max-message-width 40)       ; Max characters per line

  (knockknock-poshandler #'posframe-poshandler-window-top-right-corner)

  (knockknock-use-svg-layout t)           ; SVG layout (default)
  (knockknock-svg-icon-size 32)           ; Icon size in pixels
  (knockknock-svg-width 300)              ; Canvas width in pixels
  (knockknock-svg-padding 12)             ; Padding between icon and text
  (knockknock-max-image-size 10000)       ; Max image size (set automatically)
  :init
  (defun my-save-notification ()
    (knockknock-notify :title "File Saved"
                       :message (buffer-name)
                       :icon "fa-save"
                       :duration 2))

  :hook
  (after-save . my-save-notification))

(provide 'init-notify)
