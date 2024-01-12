;; Don't bring up key recipient dialogue.
;; Gpg settings
(use-package epa-file
  :defer t
  :config
  (setq epa-file-select-keys nil)
  (setq epa-file-encrypt-to '("liaotx2@gmail.com"))
  )

(provide 'init-gpg)
