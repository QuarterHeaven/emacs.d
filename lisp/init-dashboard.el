;; -*- lexical-binding: t -*-
(use-package dashboard
  ;; :disabled t
  :straight t
  :after (page-break-lines)
  :init
  (dashboard-setup-startup-hook)
  :hook (server-after-make-frame . dashboard-refresh-buffer)
  :config
  ;; (run-with-idle-timer 0.1 nil 'toggle-frame-maximized)
  (setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))

  ;; Set the title
  (setq dashboard-banner-logo-title "Welcome to Emacs, TakaObsid!")
  ;; Set the banner
  (setq banner '("~/.emacs.d/seth.jpeg" "~/.emacs.d/Kalan.jpeg"))
  ;; (setq dashboard-startup-banner "/Users/takaobsid/.emacs.d/seth.jpeg")

  (setq dashboard-image-banner-max-height (/ (window-pixel-height) 3))
  (setq dashboard-center-content t)
  (setq dashboard-show-shortcuts nil)

  (setq dashboard-items '((recents  . 5)
                          (bookmarks . 5)
                          (projects . 5)
                          (agenda . 5)
                          (registers . 5)))

  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-week-agenda t)
  (setq dashboard-set-navigator t)
  (setq dashboard-agenda-sort-strategy '(time-up))
  (setq dashboard-page-separator "\n\f\n")

  (defun dashboard-refresh ()
    "Refresh dashboard buffer."
    (interactive)

    (unless (get-buffer dashboard-buffer-name)
      (generate-new-buffer "*dashboard*"))
    (dashboard-set-random-banner)
    (dashboard-refresh-buffer))

  (setq tab-bar-new-tab-choice 'dashboard-refresh)
  (defun dashboard-set-random-banner ()
    (let (rd)
      (setq rd (random))
      (setq dashboard-startup-banner (nth (mod rd (length banner)) banner))))
  (dashboard-set-random-banner))

(use-package welcome-dashboard
  :disabled t
  :straight (
             :host github
             :repo "QuarterHeaven/welcome-dashboard"
             :files ("welcome-dashboard.el")
             :ensure t
             :after nerd-icons
             )
  :demand
  :init
  (setq welcome-dashboard-latitude 30.659462
        welcome-dashboard-longitude 104.065735 ;; latitude and longitude must be set to show weather information
        welcome-dashboard-use-nerd-icons t ;; Use nerd icons instead of all-the-icons
        welcome-dashboard-path-max-length 70
        welcome-dashboard-use-fahrenheit nil ;; show in celcius or fahrenheit.
        welcome-dashboard-min-left-padding 0
        welcome-dashboard-image-file "~/.emacs.d/Kalan.png"
        welcome-dashboard-image-width 408
	welcome-dashboard-image-height 289
	;; welcome-dashboard-image-height (/ (window-pixel-height) 2)
        welcome-dashboard-max-number-of-todos 5
        ;; welcome-dashboard-image-height 169
        welcome-dashboard-title "Welcome Taka. Have a great day!")
  :config
  (welcome-dashboard-create-welcome-hook))

(provide 'init-dashboard)
