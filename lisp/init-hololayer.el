(use-package holo-layer
  :straight (:repo "manateelazycat/holo-layer")
  :init
  (require 'holo-layer)
  (holo-layer-enable)
  :config
  (setq holo-layer-python-file "/home/takaobsid/.emacs.d/straight/repos/holo-layer/holo_layer.py"
	holo-layer-enable-cursor-animation t))

(provide 'init-hololayer)
