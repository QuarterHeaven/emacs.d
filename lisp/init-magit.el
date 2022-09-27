(require-package 'magit)
(require-package 'forge)

(with-eval-after-load 'magit
  (require 'forge))

(provide 'init-magit)
