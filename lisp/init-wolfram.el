;; -*- lexical-binding: t -*-
(autoload 'wolfram-mode "wolfram-mode" nil t)
(autoload 'run-wolfram "wolfram-mode" nil t)
(setq wolfram-program "/Applications/Wolfram Engine.app/Contents/MacOS/WolframKernel")
;; (add-to-list 'auto-mode-alist '("\.m$" . wolfram-mode))
(provide 'init-wolfram)
