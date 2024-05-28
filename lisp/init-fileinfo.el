(use-package file-info
  :straight (:host github :repo "artawower/file-info.el")
  :bind (("C-c C-d" . 'file-info-show))
  :config
  (setq hydra-hint-display-type 'posframe)
  (setq hydra-posframe-show-params `(:poshandler posframe-poshandler-frame-center
						 :internal-border-width 2
						 :internal-border-color "#61AFEF"
						 :left-fringe 16
						 :right-fringe 16))
  (defun file-info--low-line-replace (value)
    (cond
     ((stringp value)
      ;; COMBINING CONJOINING MACRON BELOW
      (subst-char-in-string ?_ ?︭ value))
     (t
      value)))
  (advice-add
   'file-info--get-pretty-information
   :filter-return
   #'file-info--low-line-replace))

(provide 'init-fileinfo)
