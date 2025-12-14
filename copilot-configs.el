;; -*- lexical-binding: t; -*-

(use-package copilot
  :ensure t
  :vc (:url "https://github.com/copilot-emacs/copilot.el"
            :rev :newest
            :branch "main")
  :config
  (add-hook 'prog-mode-hook 'copilot-mode)
  ;; (setq copilot-lsp-settings '(:github-enterprise (:uri "https://github.com")))
  (add-to-list 'copilot-major-mode-alist '("enh-go" . "go"))
  (add-to-list 'copilot-major-mode-alist '("enh-python" . "python"))
  (add-to-list 'copilot-major-mode-alist '("enh-rust" . "rust"))
  (add-to-list 'copilot-major-mode-alist '("enh-javascript" . "javascript"))
  (add-to-list 'copilot-major-mode-alist '("enh-typescript" . "typescript"))
  (add-to-list 'copilot-major-mode-alist '("enh-lisp" . "lisp"))

  (add-to-list 'copilot-indentation-alist '(go-mode 4))
  (add-to-list 'copilot-indentation-alist '(emacs-lisp-mode 2))
  (add-to-list 'copilot-indentation-alist '(python-mode 4))
  (add-to-list 'copilot-indentation-alist '(rst-mode 4))
  (add-to-list 'copilot-indentation-alist '(ts-mode 4))

  (define-key copilot-mode-map (kbd "<C-return>") 'copilot-accept-completion)
  )

