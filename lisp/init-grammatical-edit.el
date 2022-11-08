(add-to-list 'load-path "~/.emacs.d/site-lisp/grammatical-edit") ; add grammatical-edit to your load-path
(require 'grammatical-edit)

(dolist (hook (list
               'c-mode-common-hook
               'c-mode-hook
               'c++-mode-hook
               'java-mode-hook
               'haskell-mode-hook
               'emacs-lisp-mode-hook
               'lisp-interaction-mode-hook
               'lisp-mode-hook
               'maxima-mode-hook
               'ielm-mode-hook
               'sh-mode-hook
               'makefile-gmake-mode-hook
               'php-mode-hook
               'python-mode-hook
               'js-mode-hook
               'go-mode-hook
               'qml-mode-hook
               'jade-mode-hook
               'css-mode-hook
               'ruby-mode-hook
               'coffee-mode-hook
               'rust-mode-hook
               'qmake-mode-hook
               'lua-mode-hook
               'swift-mode-hook
               'minibuffer-inactive-mode-hook
               'typescript-mode-hook
               ))
  (add-hook hook '(lambda () (grammatical-edit-mode 1))))

(define-key grammatical-edit-mode-map (kbd "(") 'grammatical-edit-open-round)
(define-key grammatical-edit-mode-map (kbd "[") 'grammatical-edit-open-bracket)
(define-key grammatical-edit-mode-map (kbd "{") 'grammatical-edit-open-curly)
(define-key grammatical-edit-mode-map (kbd ")") 'grammatical-edit-close-round)
(define-key grammatical-edit-mode-map (kbd "]") 'grammatical-edit-close-bracket)
(define-key grammatical-edit-mode-map (kbd "}") 'grammatical-edit-close-curly)
(define-key grammatical-edit-mode-map (kbd "=") 'grammatical-edit-equal)

(define-key grammatical-edit-mode-map (kbd "%") 'grammatical-edit-match-paren)
(define-key grammatical-edit-mode-map (kbd "\"") 'grammatical-edit-double-quote)
(define-key grammatical-edit-mode-map (kbd "'") 'grammatical-edit-single-quote)

(define-key grammatical-edit-mode-map (kbd "SPC") 'grammatical-edit-space)
(define-key grammatical-edit-mode-map (kbd "RET") 'grammatical-edit-newline)

(define-key grammatical-edit-mode-map (kbd "M-o") 'grammatical-edit-backward-delete)
(define-key grammatical-edit-mode-map (kbd "C-d") 'grammatical-edit-forward-delete)
(define-key grammatical-edit-mode-map (kbd "C-k") 'grammatical-edit-kill)

(define-key grammatical-edit-mode-map (kbd "M-\"") 'grammatical-edit-wrap-double-quote)
(define-key grammatical-edit-mode-map (kbd "M-'") 'grammatical-edit-wrap-single-quote)
(define-key grammatical-edit-mode-map (kbd "M-[") 'grammatical-edit-wrap-bracket)
(define-key grammatical-edit-mode-map (kbd "M-{") 'grammatical-edit-wrap-curly)
(define-key grammatical-edit-mode-map (kbd "M-(") 'grammatical-edit-wrap-round)
(define-key grammatical-edit-mode-map (kbd "M-)") 'grammatical-edit-unwrap)

(define-key grammatical-edit-mode-map (kbd "M-p") 'grammatical-edit-jump-right)
(define-key grammatical-edit-mode-map (kbd "M-n") 'grammatical-edit-jump-left)
(define-key grammatical-edit-mode-map (kbd "M-:") 'grammatical-edit-jump-out-pair-and-newline)

(define-key grammatical-edit-mode-map (kbd "C-j") 'grammatical-edit-jump-up)

(provide 'init-grammatical-edit)
