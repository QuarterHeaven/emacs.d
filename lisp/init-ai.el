;; -*- lexical-binding: t -*-
;;; GPTEL
(use-package gptel
  :straight t
  :hook
  ((gptel-post-stream . gptel-auto-scroll)
   (gptel-post-response . gptel-end-of-response)
   (gptel-mode . gptel-set-default-directory))
  :config
  ;; (setq gptel-proxy "127.0.0.1:1081")

  (setq-default gptel-model 'deepseek-chat
        	gptel-backend
                (gptel-make-openai "DeepSeek"    
                  :host "api.deepseek.com"
                  :endpoint "/chat/completions"
                  :stream t
                  :key #'gptel-api-key-deepseek
                  :models '(deepseek-reasoner deepseek-chat))
                gptel-default-mode 'org-mode)

  (gptel-make-openai "indrin"
    :protocol "https"
    :host "llm.indrin.cn"
    :stream t
    :key #'gptel-api-key-indrin
    :models '(gpt-4o o1-mini o1-preview gpt-4o-mini deepseek-chat))
  
  (gptel-make-openai "gptapi.us"
    :protocol "https"
    :host "www.gptapi.us"
    :stream t
    :key #'gptel-api-key-gptapi
    :models '(claude-3-7-sonnet deepseek-v3 deepseek-r1 claude-3-5-sonnet claude-3-5-haiku o4-mini gpt-4.5-preview deepseek-v3.1))
  ;; Github Models offers an OpenAI compatible API
  (gptel-make-openai "Github Models" ;Any name you want
    :host "models.inference.ai.azure.com"
    :endpoint "/chat/completions"
    :stream t
    :key #'my-github-token
    :models '(gpt-4o gpt-4o-mini))
  
  

  (defun gptel-set-default-directory ()
    (unless (buffer-file-name)
      (setq default-directory "~/Documents/orgs/chats/")))

  (defun gptel-rename-chat ()
    (interactive)
    (unless gptel-mode
      (user-error "This command is intended to be used in gptel chat buffers."))
    (let ((gptel-model 'gpt-4o-mini))
      (gptel-request
          (list nil                                    ;user
                "What is the chat content?"            ;llm
                (concat "```" (if (eq major-mode 'org-mode) "org" "markdown") "\n"
                        (buffer-substring-no-properties (point-min) (point-max))
                        "\n```"))                      ;user
        :system
        (list (format                                  ;system message
               "I will provide a transcript of a chat with an LLM.  \
Suggest a short and informative name for a file to store this chat in.  \
Use the following guidelines:
- be very concise, one very short sentence at most
- no spaces, use underscores if required
- return ONLY the title, no explanation or summary
- use Chinese
- no words like 'chat history'
- append the extension .%s"
               (if (eq major-mode 'org-mode) "org" "md")))
        :callback
        (lambda (resp info)                           ;callback called with response and request info
          (if (stringp resp)
              (let ((buf (plist-get info :buffer)))
                (when (and (buffer-live-p buf)
                           (y-or-n-p (format "Rename buffer %s to %s? " (buffer-name buf) resp)))
                  (with-current-buffer buf (rename-visited-file resp))))
            (message "Error(%s): did not receive a response from the LLM."
                     (plist-get info :status)))))))
  )

(use-package aider
  :straight (:host github :repo "tninja/aider.el" :files ("aider.el"))
  :config
  (setq aider-args '("--no-auto-commits" "--model" "azure/gpt-4o-mini"))
  (setenv "AZURE_API_BASE" "https://models.inference.ai.azure.com")
  (setenv "AZURE_API_KEY" my-github-token)
  ;; Optional: Set a key binding for the transient menu
  (global-set-key (kbd "C-c a") 'aider-transient-menu))

(use-package copilot-chat
  :straight (:host github :repo "chep/copilot-chat.el" :files ("*.el"))
  :after (request org markdown-mode shell-maker)
  :config
  (setq copilot-chat-frontend 'org))

(use-package gptel-quick
  :straight (:host github :repo "karthink/gptel-quick")
  :after gptel
  :commands (gptel-quick)
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
  :disabled
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
  (setq copilot-max-char -1)
  (add-to-list 'copilot-indentation-alist '(prog-mode 2))
  (add-to-list 'copilot-indentation-alist '(org-mode 2))
  (add-to-list 'copilot-indentation-alist '(text-mode 2))
  (add-to-list 'copilot-indentation-alist '(closure-mode 2))
  (add-to-list 'copilot-indentation-alist '(emacs-lisp-mode 2))
  (setq copilot-network-proxy '(:host "127.0.0.1" :port 1081)))

;;; minuet
(use-package minuet
  :disabled
  :straight (:host github :repo "milanglacier/minuet-ai.el")
  :bind
  (("H-y" . #'minuet-complete-with-minibuffer) ;; use minibuffer for completion
   ("M-i" . #'minuet-show-suggestion) ;; use overlay for completion
   ("C-c m" . #'minuet-configure-provider)
   :map minuet-active-mode-map
   ;; These keymaps activate only when a minuet suggestion is displayed in the current buffer
   ("M-p" . #'minuet-previous-suggestion) ;; invoke completion or cycle to next completion
   ("M-n" . #'minuet-next-suggestion) ;; invoke completion or cycle to previous completion
   ("M-A" . #'minuet-accept-suggestion) ;; accept whole completion
   ;; Accept the first line of completion, or N lines with a numeric-prefix:
   ;; e.g. C-u 2 M-a will accepts 2 lines of completion.
   ("M-a" . #'minuet-accept-suggestion-line)
   ("M-e" . #'minuet-dismiss-suggestion))

  :init
  ;; if you want to enable auto suggestion.
  ;; Note that you can manually invoke completions without enable minuet-auto-suggestion-mode
  (add-hook 'prog-mode-hook #'minuet-auto-suggestion-mode)
  
  (defvar minuet-openai-compatible-options
    `(:end-point "https://api.gptapi.us/v1/chat/completions"
                 :api-key "GPTAPI_API_KEY"
                 :model "deepseek-v3"
                 :system
                 (:template minuet-default-system-template
                            :prompt minuet-default-prompt
                            :guidelines minuet-default-guidelines
                            :n-completions-template minuet-default-n-completion-template)
                 :fewshots minuet-default-fewshots
                 :chat-input
                 (:template minuet-default-chat-input-template
                            :language-and-tab minuet--default-chat-input-language-and-tab-function
                            :context-before-cursor minuet--default-chat-input-before-cursor-function
                            :context-after-cursor minuet--default-chat-input-after-cursor-function)
                 :optional nil)
    "Config options for Minuet OpenAI compatible provider.")

  :config
  ;; You can use M-x minuet-configure-provider to interactively configure provider and model
  (setq minuet-provider 'openai-compatible)

  (minuet-set-optional-options minuet-openai-fim-compatible-options :max_tokens 256)
  (minuet-set-optional-options minuet-openai-fim-compatible-options :top_p 0.9)
  
  (plist-put minuet-openai-fim-compatible-options :api-key "DEEPSEEK_API_KEY")
  
  
  (minuet-set-optional-options minuet-openai-compatible-options :max_tokens 256)
  (minuet-set-optional-options minuet-openai-compatible-options :top_p 0.9)
  
  )

;;; aidermacs
(use-package aidermacs
  :straight (:host github :repo "MatthewZMD/aidermacs")
  :bind (("C-c a" . aidermacs-transient-menu))
  :config
  (setenv "AIDER_AUTO_COMMITS" "False") ;; Disable auto commit of LLM changes
  (setenv "AIDER_CHAT_LANGUAGE" "Chinese") ;; Specify the language to use in the chat

  :custom
  (aidermacs-use-architect-mode t)
  (aidermacs-default-model "openai/deepseek-v3")
  (aidermacs-architect-model "openai/claude-3-7-sonnet")
  (aidermacs-weak-model "openai/deepseek-v3")
  (aidermacs-backend 'comint)
  )

(provide 'init-ai)
