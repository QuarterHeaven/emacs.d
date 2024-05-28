(use-package search-web
  :straight t
  :defer t
  :init
  (setq search-web-engines
	'(("Google" "http://www.google.com/search?q=%s" nil)
	  ("Youtube" "http://www.youtube.com/results?search_query=%s" nil)
	  ("Stackoveflow" "http://stackoverflow.com/search?q=%s" nil)
	  ("Sogou" "https://www.sogou.com/web?query=%s" nil)
	  ("Github" "https://github.com/search?q=%s" nil)
	  ("Melpa" "https://melpa.org/#/?q=%s" nil)
	  ("Emacs-China" "https://emacs-china.org/search?q=%s" nil)
	  ("EmacsWiki" "https://www.emacswiki.org/emacs/%s" nil)
	  ("Wiki-zh" "https://zh.wikipedia.org/wiki/%s" nil)
	  ("Wiki-en" "https://en.wikipedia.org/wiki/%s" nil)
	  ))
  :bind (("C-c w u" . browse-url)
         ("C-c w w" . search-web)
         ("C-c w p" . search-web-at-point)
         ("C-c w r" . search-web-region)))

(use-package grip-mode
  :straight t
  :hook (markdown-mode . grip-mode))

(provide 'init-web)
