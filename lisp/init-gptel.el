(use-package gptel
  :straight t
  :hook
  ((gptel-post-stream . gptel-auto-scroll)
   (gptel-post-response . gptel-end-of-response))
  :config
  (setq gptel-api-key "*****"
	gptel-proxy "127.0.0.1:1081")

  (setq-default Gptel-model "gemini-pro" ;Pick your default model
		gptel-backend (gptel-make-gemini
			       "Gemini-Pro"
			       :key "*****"
			       :stream t)))

(provide 'init-gptel)
