(use-package pocket-reader
  :ensure t
  :bind (("\C-c\C-p" . pocket-reader))
  :config
  (define-key mu4e-view-mode-map "p" 'pocket-reader-add-link)
  )
