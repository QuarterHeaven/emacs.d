(use-package dashboard
  :straight t
  :after (page-break-lines)
  :init
  (dashboard-setup-startup-hook)
  :config
  (run-with-idle-timer 0.1 nil 'toggle-frame-maximized)
  (setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))

  ;; Set the title
  (setq dashboard-banner-logo-title "Welcome to Emacs, TakaObsid!")
  ;; Set the banner
  (setq banner '("/Users/takaobsid/.emacs.d/seth.jpeg" "/Users/takaobsid/.emacs.d/Kalan.jpeg"))
  ;; (setq dashboard-startup-banner "/Users/takaobsid/.emacs.d/seth.jpeg")

  (setq dashboard-image-banner-max-height 250)
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

(provide 'init-dashboard)
