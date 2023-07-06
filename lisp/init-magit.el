(add-to-list 'load-path "~/.emacs.d/site-lisp/magit/lisp")

(with-eval-after-load 'info
  (info-initialize)
  (add-to-list 'Info-directory-list
               "~/.emacs.d/site-lisp/magit/Documentation/"))
(add-to-list 'load-path "~/.emacs.d/site-lisp/dash")
(add-to-list 'load-path "~/.emacs.d/site-lisp/transient/lisp")
(add-to-list 'load-path "~/.emacs.d/site-lisp/with-editor/lisp")
(add-to-list 'load-path "~/.emacs.d/site-lisp/ghub/lisp")
(add-to-list 'load-path "~/.emacs.d/site-lisp/forge/lisp")
(require 'magit)
(require 'forge)

(with-eval-after-load 'magit
  (use-package forge
    ;; :defer t
    :demand t
    :defines forge-topic-list-columns
    :custom-face
    (forge-topic-label ((t (:inherit variable-pitch :height 0.9 :width condensed :weight regular :underline nil))))
    :init
    (setq forge-topic-list-columns
          '(("#" 5 forge-topic-list-sort-by-number (:right-align t) number nil)
            ("Title" 60 t nil title  nil)
            ("State" 6 t nil state nil)
            ("Updated" 10 t nil updated nil)))))

(setq auth-sources '("~/.authinfo.gpg"))

(provide 'init-magit)
