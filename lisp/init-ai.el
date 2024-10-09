(use-package gptel
  :straight t
  :hook
  ((gptel-post-stream . gptel-auto-scroll)
   (gptel-post-response . gptel-end-of-response))
  :config
  ;; (setq gptel-proxy "127.0.0.1:1081")

  (setq-default Gptel-model "gpt-4o-mini" ;Pick your default model
		gptel-backend (gptel-make-openai "indrin"
				:protocol "https"
				:host "llm.indrin.cn"
				:stream t
				:key #'gptel-api-key
				:models '("o1-mini" "o1-preview" "gpt-4o-mini" "gpt-4o"))
		)
  ;; Github Models offers an OpenAI compatible API
  (gptel-make-openai "Github Models" ;Any name you want
    :host "models.inference.ai.azure.com"
    :endpoint "/chat/completions"
    :stream t
    :key #'my-github-token
    :models '("gpt-4o" "gpt-4o-mini")))

(use-package aider
  :straight (:host github :repo "tninja/aider.el" :files ("aider.el"))
  :config
  (setq aider-args '("--model" "azure/gpt-4o-mini"))
  (setenv "AZURE_API_BASE" "https://models.inference.ai.azure.com")
  (setenv "AZURE_API_KEY" my-github-token)
  ;; Optional: Set a key binding for the transient menu
  (global-set-key (kbd "C-c a") 'aider-transient-menu))

(provide 'init-ai)
