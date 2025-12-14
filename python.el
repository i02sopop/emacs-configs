;; -*- lexical-binding: t; -*-

(use-package jedi
  :ensure t
  :config
  ;; (add-hook 'python-mode-hook (lambda () (setq-local imenu-create-index-function #'python-imenu-create-flat-index)))
  (add-hook 'python-mode-hook 'jedi:setup)
  (setq jedi:complete-on-dot t))

(use-package jedi-core
  :ensure t)

(use-package lsp-jedi
  :ensure t
  :config
  (add-hook 'python-mode-hook 'lsp)

  (global-set-key (kbd "C-c C-g") 'lsp-find-definition)
  (global-set-key (kbd "C-c C-a") 'lsp-find-references)
  (global-set-key (kbd "C-c C-b") 'pop-tag-mark)
  )

(use-package anaconda-mode
  :ensure t)
