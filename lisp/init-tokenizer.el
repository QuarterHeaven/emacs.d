(when *is-a-mac*
  (use-package emt
    :straight (:host github :repo "roife/emt")
    :hook (after-init . emt-mode)
    :config
    (setq emacs-macos-tokenizer-lib-path "~/.emacs.d/modules/libEMT.dylib")
    ))

(provide 'init-tokenizer)
