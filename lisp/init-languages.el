(use-package conda
  :straight t
  :defer t
  :config
  (conda-env-initialize-interactive-shells)                        ;; support interactive shell
  (conda-env-initialize-eshell)                                    ;; support eshell
  ;; (conda-env-autoactivate-mode t)                                  ;; auto-activation
  (custom-set-variables '(conda-anaconda-home "~/miniconda3/"))     ;; specify installation directory
  )

(use-package quickrun
  :straight t
  :defer t
  :bind (("C-c r r"  . quickrun))
  :config
  (setq quickrun-focus-p nil))

;; [elec-pair]
(use-package elec-pair
  ;; :disabled
  :hook ((prog-mode conf-mode yaml-mode org-mode markdown-mode) . electric-pair-mode)
  :config
  (setq electric-pair-inhibit-predicate 'electric-pair-default-inhibit))

(use-package puni
  :straight t
  :defer t
  :bind
  ("C-c C-<backspace>" . #'puni-force-delete)
  ("C-c H-<backspace>" . #'puni-force-delete)
  (:map puni-mode-map
        ("DEL" . +puni-hungry-delete))
  :init
  (puni-global-mode)
  (add-hook 'term-mode-hook #'puni-disable-puni-mode)
  :config
  (defun +puni-hungry-delete ()
    (interactive)
    (if (looking-back "^[[:blank:]]+")
        (let* ((puni-mode nil)
               (original-func (key-binding (kbd "DEL"))))
          ;; original-func is what `DEL' would be if puni-mode were disabled
          (if (eq original-func 'delete-backward-char)
              (backward-delete-char-untabify 1)
            (call-interactively original-func)))
      (puni-backward-delete-char))))

(use-package ein
  :straight t)

(use-package protobuf-mode
  :straight t)

(use-package devdocs
  :straight t
  :bind ("C-h D" . devdocs-lookup))

(unless (package-installed-p 'clangd-inactive-regions)
  (package-vc-install "https://github.com/fargiolas/clangd-inactive-regions.el"))

(use-package clangd-inactive-regions
  :init
  (add-hook 'eglot-managed-mode-hook #'clangd-inactive-regions-mode)
  :config
  (clangd-inactive-regions-set-method "darken-foreground")
  (clangd-inactive-regions-set-opacity 0.55))

;; (use-package clangd-inactive-regions
;;   :straight (:type git :host github :repo "fargiolas/clangd-inactive-regions.el")
;;   :hook
;;   (eglot-managed-mode . clangd-inactive-regions)
;;   :config
;;   (clangd-inactive-regions-set-method "darken-foreground")
;;   (clangd-inactive-regions-set-opacity 0.55))

(use-package eglot-java
  :disabled t
  :straight (:host github :repo "yveszoundi/eglot-java")
  :hook
  ((java-mode java-ts-mode) . eglot-java-mode)
  :config
  (setq eglot-java-java-home "/nix/store/mrspaijbsp1gi69l45ifnqaa3wigjl6d-openjdk-8u362-ga/lib/openjdk")
  ;; (setq eglot-java-java-program "/etc/profiles/per-user/takaobsid/bin/java")
  (setq eglot-java-java-program "/nix/store/n7ckcm50qcfnb4m81y8xl0vhzcbnaidg-openjdk-17.0.7+7/bin/java")
  (setq eglot-java-user-init-opts-fn 'custom-eglot-java-init-opts)
  (defun custom-eglot-java-init-opts (server eglot-java-eclipse-jdt)
    "Custom options that will be merged with any default settings."
    '(:bundles ["/home/takaobsid/.emacs.d/adapter/com.microsoft.java.debug.plugin-0.52.0.jar"]
	       :settings
	       (:java
		(:configuration
		 (:runtimes [
			     (:name "JavaSE-1.8"
				    :path "/nix/store/mrspaijbsp1gi69l45ifnqaa3wigjl6d-openjdk-8u362-ga/bin/"
				    :default t)
			     (:name "JavaSE-17"
				    :path "/nix/store/n7ckcm50qcfnb4m81y8xl0vhzcbnaidg-openjdk-17.0.7+7/bin/")])))))
(add-to-list 'eglot-java-eclipse-jdt-args "-javaagent:/home/takaobsid/.m2/repository/org/projectlombok/lombok/1.18.32/lombok-1.18.32.jar")
  )

(use-package dape
  :straight (:host github :repo "svaante/dape")
  :preface
  :hook
  ;; Save breakpoints on quit
  ((kill-emacs . dape-breakpoint-save)
  ;; Load breakpoints on startup
   (after-init . dape-breakpoint-load))
  :init
  ;; (setq dape-buffer-window-arrangement 'gud)
  :config
   ;; Info buffers to the right
  (setq dape-buffer-window-arrangement 'right)

  ;; Global bindings for setting breakpoints with mouse
  (dape-breakpoint-global-mode)

  ;; To not display info and/or buffers on startup
  ;; (remove-hook 'dape-on-start-hooks 'dape-info)
  ;; (remove-hook 'dape-on-start-hooks 'dape-repl)

  ;; To display info and/or repl buffers on stopped
  (add-hook 'dape-on-stopped-hooks 'dape-info)
  (add-hook 'dape-on-stopped-hooks 'dape-repl)

  ;; Kill compile buffer on build success
  (add-hook 'dape-compile-compile-hooks 'kill-buffer)

  ;; Save buffers on startup, useful for interpreted languages
  (add-hook 'dape-on-start-hooks (lambda () (save-some-buffers t t)))

  ;; Projectile users
  ;; (setq dape-cwd-fn 'projectile-project-root)
  )

(use-package mvn-el
  :straight (:type git :host github :repo "apg/mvn-el"))

(use-package yaml-mode
  :straight t
  :hook
  (yaml-mode . (lambda ()
        (define-key yaml-mode-map "\C-m" 'newline-and-indent)))
  :config
  (add-to-list 'auto-mode-alist '("\\.\\(.yml\\|yaml\\)\\'" . yaml-mode)))

(use-package json-mode
  :straight t
  :config
  (add-to-list 'auto-mode-alist '("\\.json\\'" . json-mode)))

(use-package geiser-mit
  :straight t
  )

(use-package web-mode
  :straight t
  :init
  (define-derived-mode vue-mode web-mode "Vue")
  (add-to-list 'auto-mode-alist '("\\.vue\\'" . vue-mode))
  (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode)))

(use-package scheme-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.scm\\'" . scheme-mode))
  (add-to-list 'auto-mode-alist '("\\.ss\\'" . scheme-mode)))

;; (setq rust-prettify-symbols-alist
;;       '(("&&" . ?∧) ("||" . ?∨)
;; 	("<=" . ?≤)  (">=" . ?≥) ("!=" . ?≠)
;; 	("INFINITY" . ?∞) ("->" . ?→) ("=>" . ?⇒)
;; 	("for" . ?∀) ("in" . ?∈) ("not" . ?¬)
;; 	("true" . ?✓) ("false" . ?✗)
;; 	("None" . ?∅)
;; 	("unsafe" . ?☠) ("as" . ?≅)
;; 	(".." . ?‥) ("..." . ?…)
;; 	("fn" . ?λ)
;; 	("self" . ?𝓈) ("super" . ?⇈)
;; 	("isize" . ?ℤ) ("usize" . ?ℕ) ("bool" . ?𝔹)
;; 	("new" . ?∅)
;; 	("mut" . ?μ)
;; 	("is_empty" . ?∅)
;; 	("Vec" . ?⎕)
;; 	("Option" . ??)
;; 	("let" . ?∃)
;; 	("str" . ?𝕊)
;; 	("String" . ?𝕊))
;;       "Alist of symbol prettifications used for `prettify-symbols-alist'.")

(provide 'init-languages)
