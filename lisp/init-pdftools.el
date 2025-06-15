;; -*- lexical-binding: t -*-
(use-package tablist
  )

;; (use-package pdf-tools
;;   :straight t
;;   :hook ((pdf-tools-enabled . pdf-view-auto-slice-minor-mode)
;;          (pdf-tools-enabled . pdf-isearch-minor-mode)
;;          (pdf-tools-enabled . pdf-occur-global-minor-mode)
;;          (pdf-tools-enabled . pdf-outline-minor-mode))
;;   :mode ("\\.[pP][dD][fF]\\'" . pdf-view-mode)

;;   :bind (:map pdf-view-mode-map
;;               ([remap scroll-up-command]
;;                . pdf-view-scroll-up-or-next-page)
;;               ([remap scroll-down-command]
;;                . pdf-view-scroll-down-or-previous-page))
;;   :init
;;   (setq pdf-view-use-scaling t
;;         pdf-view-use-imagemagick nil
;;         pdf-annot-activate-created-annotations t
;; 	pdf-view-selection-style 'glyph)
;;   :config
;;   (if (not (daemonp))
;;       (pdf-tools-install)))
(use-package pdf-tools
  :straight t
  :config
  (pdf-tools-install))

(provide 'init-pdftools)
