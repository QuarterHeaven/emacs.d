(require-package 'dashboard)
(require-package 'page-break-lines)

(use-package page-break-lines
  :ensure t
  :demand t)

(dashboard-setup-startup-hook)
(setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))

;; Set the title
(setq dashboard-banner-logo-title "Welcome to Emacs, TakaObsid!")
;; Set the banner
(setq dashboard-startup-banner "/Users/takaobsid/.emacs.d/seth.jpeg")
;; Value can be
;; 'official which displays the official emacs logo
;; 'logo which displays an alternative emacs logo
;; 1, 2 or 3 which displays one of the text banners
;; "path/to/your/image.gif", "path/to/your/image.png" or "path/to/your/text.txt" which displays whatever gif/image/text you would prefer
(setq dashboard-image-banner-max-height 350)


;; Content is not centered by default. To center, set
(setq dashboard-center-content t)

;; To disable shortcut "jump" indicators for each section, set
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

(provide 'init-dashboard)
