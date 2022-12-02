(require 'websocket)
(require 'deno-bridge)
(require 'deno-bridge-jieba)

(global-set-key (kbd "M-f") 'deno-bridge-jieba-forward-word)
(global-set-key (kbd "M-b") 'deno-bridge-jieba-backward-word)

(provide 'init-deno)
