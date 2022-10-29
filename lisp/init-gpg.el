;; Don't bring up key recipient dialogue.
;; gpg settings
(require 'epa-file)
(setq epa-file-select-keys nil)
(setq epa-file-encrypt-to '("liaotx2@gmail.com"))

(provide 'init-gpg)
