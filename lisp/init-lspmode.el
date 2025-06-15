;; -*- lexical-binding: t -*-
(use-package lsp-mode
  :disabled
  :straight t
  :diminish "LSP"
  :custom
  (lsp-completion-provider :none)
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  ;; :hook ;; (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
  ;;  ((c-mode c-ts-mode) . lsp)
  ;;  ((c++-mode c++-ts-mode) . lsp)
  ;;  ((python-mode python-ts-mode) . lsp)
  ;;  ((rust-mode rust-ts-mode) . lsp)
  ;;  ((clojure-mode clojure-ts-mode) . lsp)
  ;;  ;; if you want which-key integration
  ;;  ;; (lsp-mode . lsp-enable-which-key-integration)
  ;;  )
  (defun my/lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(;; orderless
            hotfuzz
            ))) ;; Configure orderless
  :hook ((lsp-mode . lsp-diagnostics-mode)
         (lsp-mode . lsp-enable-which-key-integration)
	 ;; ((web-mode
	 ;;   tsx-ts-mode
         ;;   typescript-ts-mode
         ;;   js-ts-mode) . lsp-deferred)
	 (lsp-completion-mode . my/lsp-mode-setup-completion))
  :commands lsp
  :bind
  (:map lsp-mode-map
	("M-RET" . lsp-execute-code-action))

  :config
  (setq lsp-rust-analyzer-inlay-hints-mode t
	lsp-rust-analyzer-server-display-inlay-hints t
	;; core
	lsp-enable-xref t                   ; Use xref to find references
	lsp-auto-configure t
	lsp-enable-xref t
	lsp-enable-imenu t
	;; completion
	lsp-completion-enable t
	lsp-completion-enable-additional-text-edit t
	lsp-completion-show-kind t
	;; headerline
	)
  )
                         

;; optionally
(use-package lsp-ui
  :straight t
  :commands lsp-ui-mode)
(use-package lsp-treemacs :commands lsp-treemacs-errors-list)

(use-package lsp-java
  :disabled
  :straight t
  :hook
  ((java-mode java-ts-mode) . lsp)
  :init
  (setq lsp-java-vmargs '("-noverify" "-Xmx1G" "-XX:+UseG1GC" "-XX:+UseStringDeduplication" "-javaagent:/Users/takaobsid/.jdks/lombok/share/java/lombok.jar" "--add-opens" "java.base/sun.reflect.generics.reflectiveObjects=ALL-UNNAMED" "--add-opens" "java.base/com.sun.crypto.provider=ALL-UNNAMED"))
  ;; (setq lsp-java-java-path "/nix/store/zmj3m7wrgqf340vqd4v90w8dw371vhjg-openjdk-17.0.7+7/lib/openjdk/bin/java")
  ;; (setq lsp-java-java-path "/nix/store/n7ckcm50qcfnb4m81y8xl0vhzcbnaidg-openjdk-17.0.7+7/bin/java")
  :config
  (setq lsp-java-configuration-runtimes '[(:name "JavaSE-17"
                                                 :path "~/.jdks/17.0.12/zulu-17.jdk/Contents/Home"
					      :default t)
    			                  ;; (:name "JavaSE-1.8"
					  ;;        :path "~/.jdks/8.0.402/"
					  ;;        :default t)
                                          ;; (:name "JavaSE-21"
                                          ;;        :path "~/.jdks/21.0.4/zulu-21.jdk/Contents/Home"
                                          ;;        ;; :default t
                                          ;;        )
                                          ])
  (setq lsp-java-jdt-ls-command "/Users/takaobsid/.jdks/jdtls/bin/jdtls")
  (advice-add 'lsp :before (lambda (&rest _args) (eval '(setf (lsp-session-server-id->folders (lsp-session)) (ht))))))


;; optionally if you want to use debugger
(use-package dap-mode)
;; (use-package dap-LANGUAGE) to load the dap adapter for your language
(use-package dap-java
  :config
  (setq dap-java-java-command "/etc/profiles/per-user/takaobsid/bin/java"))

;; optional if you want which-key integration
;; (use-package which-key
;;     :config
;;     (which-key-mode))

(use-package lsp-java-boot
  :disabled
  :hook
  (lsp-mode . lsp-lens-mode)
  ((java-mode java-ts-mode) . lsp-java-boot-lens-mode))

(use-package emacs-lsp-booster
  :straight (:host github :repo "blahgeek/emacs-lsp-booster")
  :config
  (defun lsp-booster--advice-json-parse (old-fn &rest args)
    "Try to parse bytecode instead of json."
    (or
     (when (equal (following-char) ?#)
       (let ((bytecode (read (current-buffer))))
	 (when (byte-code-function-p bytecode)
           (funcall bytecode))))
     (apply old-fn args)))
  (advice-add (if (progn (require 'json)
			 (fboundp 'json-parse-buffer))
                  'json-parse-buffer
		'json-read)
              :around
              #'lsp-booster--advice-json-parse)

  (defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
    "Prepend emacs-lsp-booster command to lsp CMD."
    (let ((orig-result (funcall old-fn cmd test?)))
      (if (and (not test?)                             ;; for check lsp-server-present?
               (not (file-remote-p default-directory)) ;; see lsp-resolve-final-command, it would add extra shell wrapper
               lsp-use-plists
               (not (functionp 'json-rpc-connection))  ;; native json-rpc
               (executable-find "emacs-lsp-booster"))
          (progn
            (message "Using emacs-lsp-booster for %s!" orig-result)
            (cons "emacs-lsp-booster" orig-result))
	orig-result)))
  (advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command))

(provide 'init-lspmode)
