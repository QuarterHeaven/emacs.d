;; -*- lexical-binding: t -*-
(setq read-process-output-max (* 1024 1024))

;;; xref
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

;;; flymake
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
	  (note "" compilation-info))
        flymake-show-diagnostics-at-end-of-line nil)
  )

;;; eglot
(use-package eglot
  ;; :disabled
  :straight t
  :after flymake
  :hook ((c-ts-mode
	  c++-ts-mode
	  clojure-ts-mode
	  haskell-ts-mode
	  python-ts-mode
	  rust-ts-mode
	  lua-ts-mode
	  ;; java-ts-mode
	  typst-ts-mode
	  typescript-ts-mode
	  vue-ts-mode
	  nix-ts-mode
	  web-mode
	  ) . eglot-ensure)
  
  (eglot-managed-mode . eglot-inlay-hints-mode)
  :bind
  (:map eglot-mode-map
	("M-RET" . eglot-code-actions))
  
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
  
  (defun my/nix-clangd-cmd (&rest _args)
    "Use `nix-shell -p llvmPackages_15.clangd` to run clangd."
    (list "nix" "shell"
          "nixpkgs#llvmPackages_latest.clang-tools"
          "--command" "clangd" "-j=4" "--clang-tidy"))
  
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs '(typst-ts-mode . ("tinymist")))
    ;;delance 現在已經發佈到 npm 了哦，npm i -g @delance/runtime 就可以直接用 delance-langserver --stdio
    (add-to-list 'eglot-server-programs '((python-mode python-ts-mode) . ("delance-langserver" "--stdio")))
    (add-to-list 'eglot-server-programs '(nix-ts-mode . ("nil")))
    
    (add-to-list 'eglot-server-programs
		 `((vue-mode vue-ts-mode typescript-ts-mode typescript-mode) . ("vue-language-server" "--stdio" :initializationOptions ,(vue-eglot-init-options))))
    (add-to-list 'eglot-server-programs '((c-ts-mode c++-ts-mode) . my/nix-clangd-cmd))
    )

  :config
  (require 'clangd-inactive-regions)
  (setq eglot-connect-timeout 10
	eglot-events-buffer-config '(:size 0 :format full)
        eglot-autoshutdown t
        ;; use global completion styles
        completion-category-defaults nil
        eglot-code-action-indications '(eldoc-hint margin)
        eglot-code-action-indicator "󱧡")

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

;;; eglot-java
(use-package eglot-java
  ;; :disabled t
  :straight (:host github :repo "yveszoundi/eglot-java")
  :hook		       
  ((java-mode java-ts-mode) . eglot-java-mode)
  
  :config
  (custom-set-variables '(eglot-java-server-install-dir "~/.jdks/jdtls")
			'(eglot-java-eclipse-jdt-cache-directory "~/.emacs.d/eglot-java-eclipse-jdt/cache")
			'(eglot-java-eclipse-jdt-config-directory "~/.emacs.d/eglot-java-eclipse-jdt/config")
                        '(eglot-java-java-home "~/.jdks/17.0.12/zulu-17.jdk/Contents/Home"))
  (defcustom java-lombok-path (expand-file-name "~/.jdks/lombok/share/java/lombok.jar")
    "The path to the lombok library."
    :group 'eglot-java
    :type '(string))

  (defcustom dape-jdtls-java-debug-plugin-jar (expand-file-name "~/.jdks/java-debug/share/vscode/extensions/vscjava.vscode-java-debug/server/com.microsoft.java.debug.plugin-0.50.0.jar")
    "The path to java debug plugin."
    :group 'eglot-java
    :type '(string))

  (defcustom dape-jdtls-java-test-plugin-jar (expand-file-name "~/.jdks/java-test/share/vscode/extensions/vscjava.vscode-java-test/server/com.microsoft.java.test.plugin-0.40.1.jar
")
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
		(:configuration (:runtimes [(:name "JavaSE-17"
                                                   :path "~/.jdks/17.0.12/zulu-17.jdk/Contents/Home"
					           :default t)])
                                :saveActions (:organizeImports t)
                                :format (:settings
			                 (:url "https://raw.githubusercontent.com/google/styleguide/gh-pages/eclipse-java-google-style.xml")
			                 :enabled t
			                 :insertSpaces t
			                 :tabSize 4)
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

;;; [corfu] compleletion frontend 
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
              ;; ("RET" . newline)
	      ("RET" . corfu-complete)
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

;;; cape
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

;;; company
(use-package company
  ;; :disabled
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
	company-idle-delay 0.3
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
  :straight t
  :after corfu
  :demand t
  :config
  ;; (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)
  )

;;; lsp-proxy
(use-package lsp-proxy
  :disabled
  :straight (:host github :repo "jadestrong/lsp-proxy"
		   :files ("lsp-proxy.el" "lsp-proxy")
                   :pre-build (("cargo" "build" "--release") ("cp" "./target/release/lsp-proxy" "./")))
  :bind
  (:map lsp-proxy-mode-map
	("M-RET" . lsp-proxy-execute-code-action))
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
    ) . lsp-proxy-mode)
  (lsp-proxy-mode . lsp-proxy-inlay-hints-mode)
  )


;;; auto insert
(use-package auto-insert
  :after (projectile)
  :init
  (auto-insert-mode 1)
  (setq auto-insert-query nil)
  (defun taka/java-auto-insert ()
    "Auto insert java template while creating new java file."
    (let* ((project-root (projectile-project-root))
           (file-path (file-truename (file-name-directory (buffer-file-name))))
	   (src-path "src/main/java/")
           ;; 查找 src/main/java/ 在文件路径中的位置
           (src-pos (string-match (concat "/" (regexp-quote src-path)) file-path))
           ;; 提取 src/main/java/ 之后的相对路径
           (relative-path (if src-pos
                              (substring file-path (+ src-pos (length src-path) 1))
                            ""))
           (package-name
            (if (not (string-empty-p relative-path))
		(replace-regexp-in-string "/" "." (file-name-directory relative-path))
              ""))
           (package-name-clean
            (if (and package-name (not (string-empty-p package-name)))
		(replace-regexp-in-string "\\.$" "" package-name)  ;; 去掉末尾的点
              ""))
           (class-name (file-name-base (buffer-file-name))))
      (insert
       (if (not (string-empty-p package-name-clean))
	   (format "package %s;\n\npublic class %s {\n\n}\n" package-name-clean class-name)
	 (format "public class %s {\n\n}\n" class-name))
       )))

  ;; 将 Java 文件与模板关联
  (define-auto-insert '("\\.java\\'" . "Java class")
    'taka/java-auto-insert))

(provide 'init-completion)
