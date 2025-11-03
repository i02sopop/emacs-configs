(use-package copilot
  :ensure t
  :vc (:url "https://github.com/copilot-emacs/copilot.el"
            :rev :newest
            :branch "main")
  :config
  (add-hook 'prog-mode-hook 'copilot-mode)
  ;; (setq copilot-lsp-settings '(:github-enterprise (:uri "https://github.com")))
  (add-to-list 'copilot-major-mode-alist '("enh-go" . "go"))
  )
