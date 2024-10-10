(use-package dict-line
  :straight (:host github :repo "ISouthRain/dict-line")
  :diminish
  (dict-line-mode "🗺️")
  :defer 2
  :custom
  (dict-line-dict-directory "~/.emacs.d/submodules/vscode_english_chinese_dictionary/src/词典数据/")
  (dict-line-more-icon " ⚔️ ")
  (dict-line-idle-delay 1)
  :config
  (global-dict-line-mode))


(provide 'init-dictionary)
