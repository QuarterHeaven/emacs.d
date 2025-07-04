;; -*- lexical-binding: t -*-
(use-package dict-line
  :straight (:host github :repo "ISouthRain/dict-line")
  :diminish
  (dict-line-mode "🗺️")
  :defer 2
  :custom
  (dict-line-dict-directory "~/.emacs.d/submodules/vscode_english_chinese_dictionary/src/词典数据/")
  (dict-line-more-icon " ⚔️ ")
  (dict-line-idle-delay 1)
  (dict-line-display #'dict-line--posframe)
  (dict-line-posframe-location #'posframe-poshandler-window-bottom-right-corner)
  :config
  (dict-line-mode t))


(provide 'init-dictionary)
