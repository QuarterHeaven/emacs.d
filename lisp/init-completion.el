(setq read-process-output-max (* 1024 1024))

(use-package xref
  :config
  (setq
   xref-search-program 'ripgrep
   xref-show-definitions-function #'xref-show-definitions-completing-read
   xref-show-xrefs-function #'xref-show-definitions-completing-read
   xref-history-storage 'xref-window-local-history)

  (defadvice! +xref--push-marker-stack-a (&rest rest)
    :before '(find-function consult-imenu consult-ripgrep citre-jump)
    (xref-push-marker-stack (point-marker)))
  )

(use-package eglot
  :straight t
  :hook ((c-ts-mode
	  c++-ts-mode
	  clojure-ts-mode
	  haskell-ts-mode
	  python-ts-mode
	  rust-ts-mode
	  ;; java-ts-mode
	  typst-ts-mode
	  typescript-ts-mode
	  nix-ts-mode) . eglot-ensure)
  (eglot-managed-mode . eglot-inlay-hints-mode)
  :init
    ;;   (defun jdtls-initialization-options ()
    ;;   (let ((setting-json-file ;; (file-name-concat user-emacs-directory "lsp-config" "jdtls.json")
    ;; 			       "/home/takaobsid/.emacs.d/lsp-config/jdtls.json"))
    ;; 	(with-temp-buffer
    ;; 	  (insert-file-contents setting-json-file)
    ;; 	  (json-parse-buffer :object-type 'plist :false-object :json-false))))

    ;; (cl-defmethod eglot-initialization-options (server &context (major-mode java-mode))
    ;;   (jdtls-initialization-options))

    ;; (cl-defmethod eglot-initialization-options (server &context (major-mode java-ts-mode))
    ;;   (jdtls-initialization-options))

  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs '(typst-ts-mode . ("tinymist")))
    ;;delance 現在已經發佈到 npm 了哦，npm i -g @delance/runtime 就可以直接用 delance-langserver --stdio
    (add-to-list 'eglot-server-programs '((python-mode python-ts-mode) . ("delance-langserver" "--stdio")))
    (add-to-list 'eglot-server-programs '(nix-ts-mode . ("nil")))
    ;; (add-to-list 'eglot-server-programs
    ;; 		 `((java-mode java-ts-mode) .
    ;; 		   ("jdtls"
    ;;                 :initializationOptions
    ;;                 (:bundles ["/home/takaobsid/.emacs.d/adapter/com.microsoft.java.debug.plugin-0.52.0.jar"]))))

    (add-to-list 'eglot-server-programs
		 `((java-mode java-ts-mode) "jdtls"
                   "-configuration" ,(expand-file-name "cache/language-server/java/jdtls/config_linux" user-emacs-directory)
                   "-data" ,(expand-file-name "cache/java-workspace" user-emacs-directory)
                   ,(concat "--jvm-arg=-javaagent:" (expand-file-name "~/.m2/repository/org/projectlombok/lombok/1.18.32/lombok-1.18.32.jar"))
		   :initializationOptions
		   (:bundles ["/home/takaobsid/.emacs.d/adapter/com.microsoft.java.debug.plugin-0.52.0.jar"]
			     :settings
			     (:java
			      (:configuration
			       (:runtimes [
					   (:name "JavaSE-1.8"
						  :path "/nix/store/mrspaijbsp1gi69l45ifnqaa3wigjl6d-openjdk-8u362-ga/jre/"
						  :default t)
					   (:name "JavaSE-17"
						  :path "/nix/store/n7ckcm50qcfnb4m81y8xl0vhzcbnaidg-openjdk-17.0.7+7/")])))))))

  :config
  (require 'clangd-inactive-regions)
  (setq eglot-events-buffer-size 0
        eglot-connect-timeout 10
        eglot-autoshutdown t
        ;; use global completion styles
        completion-category-defaults nil)
  )

(use-package eglot-booster
  :straight (:host github :repo "jdtsmith/eglot-booster")
	:after eglot
	:config	(eglot-booster-mode))

(use-package consult-eglot
  :after consult eglot
  :straight t
  :bind (:map eglot-mode-map
              ([remap xref-find-apropos] . consult-eglot-symbols)))

;; [corfu] compleletion frontend
(use-package corfu
  :straight (:files (:defaults "extensions/*.el"))
  :hook (;; ((prog-mode conf-mode yaml-mode shell-mode eshell-mode) . corfu-mode)
	 (after-init . global-corfu-mode)
         ((eshell-mode shell-mode) . (lambda () (setq-local corfu-auto nil)))
         (minibuffer-setup . corfu-enable-in-minibuffer))
  :bind (:map corfu-map
              ("s-m" . corfu-move-to-minibuffer)
              ("RET" . newline)
	      ;; ("SPC" . corfu-insert-separator)
	      )

  :config
  (setq corfu-cycle t                ;; Enable cycling for `corfu-next/previous'
        corfu-auto t                 ;; Enable auto completion
	;; corfu-separator ?\s
	corfu-separator ?&
	corfu-auto-prefix 2          ;; minimun prefix to enable completion
        corfu-preview-current nil
        corfu-auto-delay 0.1
	corfu-popupinfo-delay 0.2
	corfu-echo-mode t)

  ;; Transfer completion to the minibuffer
  (defun corfu-move-to-minibuffer ()
    (interactive)
    (let ((completion-extra-properties corfu--extra)
          completion-cycle-threshold completion-cycling)
      (apply #'consult-completion-in-region completion-in-region--data)))

  ;; Completing in the minibuffer
  (defun corfu-enable-in-minibuffer ()
    "Enable Corfu in the minibuffer if `completion-at-point' is bound."
    (when (where-is-internal #'completion-at-point (list (current-local-map)))
      (corfu-mode 1))))


(use-package corfu-history
  :after corfu
  :init
  (corfu-history-mode 1)
  :config
  (with-eval-after-load 'savehist
    (cl-pushnew 'corfu-history savehist-additional-variables))
  )


(use-package corfu-popupinfo
  :after corfu)

(use-package cape
  :straight t
  :init
  (require 'cape-char)
  (require 'cape-keyword)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-tex)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-elisp-symbol)
  (add-to-list 'completion-at-point-functions #'cape-tex))

(use-package kind-icon
  :straight t
  :after corfu
  :demand t
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(provide 'init-completion)
