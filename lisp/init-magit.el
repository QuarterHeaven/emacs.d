(require-package 'magit)
(require-package 'forge)

(with-eval-after-load 'magit
  (require 'forge))

(use-package forge
      :demand t
      :defines forge-topic-list-columns
      :custom-face
      (forge-topic-label ((t (:inherit variable-pitch :height 0.9 :width condensed :weight regular :underline nil))))
      :init
      (setq forge-topic-list-columns
            '(("#" 5 forge-topic-list-sort-by-number (:right-align t) number nil)
              ("Title" 60 t nil title  nil)
              ("State" 6 t nil state nil)
              ("Updated" 10 t nil updated nil))))

(setq auth-sources '("~/.authinfo.gpg"))

(provide 'init-magit)
