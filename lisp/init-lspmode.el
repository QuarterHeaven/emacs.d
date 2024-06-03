(use-package lsp-mode
  :straight t
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
  :commands lsp
  :config
  (setq lsp-rust-analyzer-inlay-hints-mode t)
  (setq lsp-rust-analyzer-server-display-inlay-hints t))

;; optionally
(use-package lsp-ui
  :straight t
  :commands lsp-ui-mode)
(use-package lsp-treemacs :commands lsp-treemacs-errors-list)

(use-package lsp-java
  :straight t
  :hook
  ((java-mode java-ts-mode) . lsp)
  :init
  (setq lsp-java-vmargs '("-noverify" "-Xmx1G" "-XX:+UseG1GC" "-XX:+UseStringDeduplication" "-javaagent:/home/takaobsid/.m2/repository/org/projectlombok/lombok/1.18.32/lombok-1.18.32.jar"))
  (setq lsp-java-java-path "/nix/store/zmj3m7wrgqf340vqd4v90w8dw371vhjg-openjdk-17.0.7+7/lib/openjdk/bin/java")
  ;; (setq lsp-java-java-path "/nix/store/n7ckcm50qcfnb4m81y8xl0vhzcbnaidg-openjdk-17.0.7+7/bin/java")
  :config
  (setq lsp-java-configuration-runtimes '[;; (:name "JavaSE-17"
                                          ;;        :path "/nix/store/n7ckcm50qcfnb4m81y8xl0vhzcbnaidg-openjdk-17.0.7+7"
					  ;; 	 )
    			                  (:name "JavaSE-1.8"
						 :path "/nix/store/mrspaijbsp1gi69l45ifnqaa3wigjl6d-openjdk-8u362-ga/"
						 :default t)]))

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
