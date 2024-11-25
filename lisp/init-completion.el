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
    (xref-push-marker-stack (point-marker))))

(use-package flymake
  :straight t
  :config
  (setq flymake-start-on-flymake-mode nil
        flymake-no-changes-timeout nil
        flymake-start-on-save-buffer t
	flymake-indicator-type 'margins
	flymake-margin-indicators-string
	`((error "" compilation-error)
	  (warning "" compilation-warning)
	  (note "" compilation-info))))

(use-package eglot
  ;; :disabled
  :straight t
  :after flymake
  :hook ;; ((c-ts-mode
  ;; 	  c++-ts-mode
  ;; 	  clojure-ts-mode
  ;; 	  haskell-ts-mode
  ;; 	  python-ts-mode
  ;; 	  rust-ts-mode
  ;; 	  lua-ts-mode
  ;; 	  ;; java-ts-mode
  ;; 	  typst-ts-mode
  ;; 	  typescript-ts-mode
  ;; 	  ;; vue-ts-mode
  ;; 	  nix-ts-mode
  ;; 	  web-mode
  ;; 	  ) . eglot-ensure)
  
  (eglot-managed-mode . eglot-inlay-hints-mode)
  :init
  
  (defun vue-eglot-init-options ()
    (let ((tsdk-path (expand-file-name
                      "lib"
                      ;; (shell-command-to-string "npm list --global --parseable typescript | head -n1 | tr -d \"\n\"")
		      ;; for mac
		      (string-trim-right (shell-command-to-string "npm list --global --parseable typescript | head -n1")))))
      `(:typescript (:tsdk ,tsdk-path
                           :languageFeatures (:completion
                                              (:defaultTagNameCase "both"
                                                                   :defaultAttrNameCase "kebabCase"
                                                                   :getDocumentNameCasesRequest nil
                                                                   :getDocumentSelectionRequest nil)
                                              :diagnostics
                                              (:getDocumentVersionRequest nil))
                           :documentFeatures (:documentFormatting
                                              (:defaultPrintWidth 100
                                                                  :getDocumentPrintWidthRequest nil)
                                              :documentSymbol t
                                              :documentColor t))
		    :vue (:hybridMode :json-false)
		    )))
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs '(typst-ts-mode . ("tinymist")))
    ;;delance 現在已經發佈到 npm 了哦，npm i -g @delance/runtime 就可以直接用 delance-langserver --stdio
    (add-to-list 'eglot-server-programs '((python-mode python-ts-mode) . ("delance-langserver" "--stdio")))
    (add-to-list 'eglot-server-programs '(nix-ts-mode . ("nil")))
    
    (add-to-list 'eglot-server-programs
		 `((vue-mode vue-ts-mode typescript-ts-mode typescript-mode) . ("vue-language-server" "--stdio" :initializationOptions ,(vue-eglot-init-options))))
    )

  :config
  (require 'clangd-inactive-regions)
  (setq eglot-connect-timeout 10
	eglot-events-buffer-config '(:size 0 :format full)
        eglot-autoshutdown t
        ;; use global completion styles
        completion-category-defaults nil)

  ;; solve flymake issue
  (cl-defmethod eglot-handle-notification :after
    (_server (_method (eql textDocument/publishDiagnostics)) &key uri
             &allow-other-keys)
    (when-let ((buffer (find-buffer-visiting (eglot-uri-to-path uri))))
      (with-current-buffer buffer
	(if (and (eq nil flymake-no-changes-timeout)
		 (not (buffer-modified-p)))
            (flymake-start t)))))

  (defvar jsonrpc-log-event-p nil)
  (defun jsonrpc--log-event-advice (f &rest args)
    (if jsonrpc-log-event-p (apply f args)))
  (advice-add #'jsonrpc--log-event :around #'jsonrpc--log-event-advice)
  )

(use-package eglot-java
  :disabled
  :straight (:host github :repo "yveszoundi/eglot-java")
  :hook		       
  ((java-mode java-ts-mode) . eglot-java-mode)
  
  :config
  (custom-set-variables '(eglot-java-server-install-dir "~/.jdks/jdtls")
			'(eglot-java-eclipse-jdt-cache-directory "~/.emacs.d/eglot-java-eclipse-jdt/cache")
			'(eglot-java-eclipse-jdt-config-directory "~/.emacs.d/eglot-java-eclipse-jdt/config"))
  (defcustom java-lombok-path (expand-file-name "~/.jdks/lombok/share/java/lombok.jar")
    "The path to the lombok library."
    :group 'eglot-java
    :type '(string))

  (defcustom dape-jdtls-java-debug-plugin-jar (expand-file-name "~/.jdks/java-debug/share/vscode/extensions/vscjava.vscode-java-debug/server/com.microsoft.java.debug.plugin-0.50.0.jar")
    "The path to java debug plugin."
    :group 'eglot-java
    :type '(string))

  (defcustom dape-jdtls-java-test-plugin-jar (expand-file-name "~/.jdks/java-test/share/vscode/extensions/vscjava.vscode-java-test/server/com.microsoft.java.test.plugin-0.40.1.jar")
    "The path to java test plugin."
    :group 'eglot-java
    :type '(string))
  
  (setq eglot-java-eclipse-jdt-args (cons (concat "-javaagent:" java-lombok-path)
					  '("-Xmx1G" "--add-modules=ALL-SYSTEM"
					    "--add-opens" "java.base/java.util=ALL-UNNAMED"
					    "--add-opens" "java.base/java.lang=ALL-UNNAMED"
					    "--add-opens" "java.base/sun.reflect.generics.reflectiveObjects=ALL-UNNAMED"
					    "--add-opens" "java.base/com.sun.crypto.provider=ALL-UNNAMED"
					    )))
  (setq eglot-java-user-init-opts-fn 'custom-eglot-java-init-opts)
  (defun custom-eglot-java-init-opts (server eglot-java-eclipse-jdt)
    "Custom options that will be merged with any default settings."
    `(:bundles [,dape-jdtls-java-debug-plugin-jar ,dape-jdtls-java-test-plugin-jar]
	       :settings
	       (:java
		(:format (:settings
			  (:url "https://raw.githubusercontent.com/google/styleguide/gh-pages/eclipse-java-google-style.xml")
			  :enabled t)
			 :inlayHints (:parameterNames (:enabled "all")))))))

;; (use-package dape-jdtls
;;   :after dape
;;   :ensure nil
;;   :load-path "~/.emacs.d/site-lisp"
;;   :init
;;   (require 'dape-jdtls)
;;   (add-to-list 'dape-configs
;;                `(jdtls
;;                  modes (java-mode java-ts-mode)
;;                  fn dape-jdtls-complete-config
;;                  :args "--add-opens java.base/sun.reflect.generics.reflectiveObjects=ALL-UNNAMED --add-opens
;; java.base/com.sun.crypto.provider=ALL-UNNAMED"
;;                  :stopOnEntry nil))

;;   ;; (require 'eglot)
;;   ;; (add-to-list 'eglot-server-programs
;;   ;;              `((java-mode java-ts-mode) .
;;   ;;                ("jdtls"
;;   ;;                 :initializationOptions
;;   ;;                 (:bundles [,dape-jdtls-java-debug-plugin-jar
;;   ;;                            ,dape-jdtls-java-test-plugin-jar]))))
;;   )

(use-package eglot-booster
  :straight (:host github :repo "jdtsmith/eglot-booster")
  :after eglot
  :config
  (eglot-booster-mode)
  (setq eglot-booster-io-only t)
  )

(use-package consult-eglot
  :after consult eglot
  :straight t
  :bind (:map eglot-mode-map
              ([remap xref-find-apropos] . consult-eglot-symbols)))

;; [corfu] compleletion frontend
(use-package corfu
  :disabled
  :straight (:files (:defaults "extensions/*.el"))
  :hook (;; ((prog-mode conf-mode yaml-mode shell-mode eshell-mode) . corfu-mode)
	 (after-init . global-corfu-mode)
	 (after-init . corfu-popupinfo-mode)
         ((eshell-mode shell-mode) . (lambda () (setq-local corfu-auto nil)))
         (minibuffer-setup . corfu-enable-in-minibuffer))
  :bind (:map corfu-map
              ("s-m" . corfu-move-to-minibuffer)
              ("RET" . newline)
	      ;; ("RET" . corfu-complete)
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

(use-package nerd-icons-corfu
  :straight t
  :after corfu
  :init
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))


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

(use-package company
  :straight t
  :hook ((after-init . global-company-mode))
  :bind (:map company-active-map
	      ("TAB" . company-indent-or-complete-common)
	      )
  :custom
  (company-transformers '(company-sort-prefer-same-case-prefix 
			  company-sort-by-occurrence
                          company-sort-by-backend-importance))
  :config
  ;; (add-to-list 'company-frontends #'company-preview-frontend)
  (setq company-minimum-prefix-length 2
	company-idle-delay 0
	company-tooltip-idle-delay 0)
  )

(use-package company-quickhelp
  :straight t
  :after (company)
  :hook ((global-company-mode . company-quickhelp-mode)))

(use-package company-box
  :straight t
  :hook (company-mode . company-box-mode)
  :config
  (defun company-box-icons--lsp-copilot (candidate)
    (-when-let* ((copilot-item (get-text-property 0 'lsp-copilot--item candidate))
                 (lsp-item (plist-get copilot-item :item))
                 (kind-num (plist-get lsp-item :kind)))
      (alist-get kind-num company-box-icons--lsp-alist)))

  (setq company-box-icons-functions
	   (cons #'company-box-icons--lsp-copilot company-box-icons-functions))
  )

(use-package kind-icon
  :disabled
  :straight t
  :after corfu
  :demand t
  :config
  ;; (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)
  )

;;; codeium
(use-package codeium
  :disabled
  :straight '(:type git :host github :repo "Exafunction/codeium.el")
  
  :init
  ;; use globally
  (add-to-list 'completion-at-point-functions #'codeium-completion-at-point)

  :config
  (setq use-dialog-box nil)
  (setq codeium-mode-line-enable
        (lambda (api) (not (memq api '(CancelRequest Heartbeat AcceptCompletion)))))
  (add-to-list 'mode-line-format '(:eval (car-safe codeium-mode-line)) t)
  (setq codeium-api-enabled
        (lambda (api)
          (memq api '(GetCompletions Heartbeat CancelRequest GetAuthToken RegisterUser auth-redirect AcceptCompletion))))
  (defun my-codeium/document/text ()
    (buffer-substring-no-properties (max (- (point) 3000) (point-min)) (min (+ (point) 1000) (point-max))))
  (defun my-codeium/document/cursor_offset ()
    (codeium-utf8-byte-length
     (buffer-substring-no-properties (max (- (point) 3000) (point-min)) (point))))
  (setq codeium/document/text 'my-codeium/document/text)
  (setq codeium/document/cursor_offset 'my-codeium/document/cursor_offset))

;;; copilot
(use-package copilot
  :straight (:host github :repo "copilot-emacs/copilot.el" :files ("*.el"))
  :ensure t
  :hook
  (prog-mode . copilot-mode)
  :bind (:map copilot-completion-map
	      ("<right>" . #'copilot-accept-completion)
	      ("C-f" . #'copilot-accept-completion)
	      ("M-<right>" . #'copilot-accept-completion-by-word)
	      ("M-f" . #'copilot-accept-completion-by-word)
	      ("C-e" . #'copilot-accept-completion-by-line)
	      ("<end>" . #'copilot-accept-completion-by-line)
	      ("C-g" . #'copilot-clear-overlay))
  :config
  (add-to-list 'copilot-indentation-alist '(prog-mode 2))
  (add-to-list 'copilot-indentation-alist '(org-mode 2))
  (add-to-list 'copilot-indentation-alist '(text-mode 2))
  (add-to-list 'copilot-indentation-alist '(closure-mode 2))
  (add-to-list 'copilot-indentation-alist '(emacs-lisp-mode 2))
  (setq copilot-network-proxy '(:host "127.0.0.1" :port 1081)))

;;; lsp-copilot
(use-package lsp-copilot
  :straight (:host github :repo "jadestrong/lsp-copilot"
		   :files ("lsp-copilot.el" "lsp-copilot")
                   :pre-build (("cargo" "build" "--release") ("cp" "./target/release/lsp-copilot" "./")))
  :hook
  ((c-ts-mode
    c++-ts-mode
    clojure-ts-mode
    haskell-ts-mode
    python-ts-mode
    rust-ts-mode
    lua-ts-mode
    java-ts-mode
    typst-ts-mode
    typescript-ts-mode
    vue-ts-mode
    nix-ts-mode
    web-mode
    ) . lsp-copilot-mode)
  ;; (lsp-copilot-mode . lsp-copilot-inlay-hints-mode)
  )

(provide 'init-completion)
