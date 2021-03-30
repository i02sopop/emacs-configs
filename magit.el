(use-package magit
  :ensure t
  :config
  (global-set-key (kbd "C-c g") 'magit-status)
  )

(use-package magit-org-todos
  :ensure t)
