;; -*- lexical-binding: t; -*-

(use-package cargo
  :ensure t)

(use-package cargo-mode
  :ensure t)

(use-package cargo-transient
  :ensure t)

(use-package flycheck-rust
  :ensure t)

(use-package racer
  :ensure t)

(use-package rust-auto-use
  :ensure t)

(use-package rust-mode
  :ensure t
  :config
  (add-hook 'rust-mode-hook
			(defun rust-init-config ()
			  "Set the init configuration for rust"
			  (display-line-numbers-mode)
			  (auto-complete-mode -1)
			  (hs-minor-mode 1)
			  (hs-hide-all)))

  (define-key rust-mode-map (kbd "C-c C-j") 'lsp-find-definition)
  (define-key rust-mode-map (kbd "C-c C-r") 'lsp-find-references)
  (define-key rust-mode-map (kbd "C-c C-b") 'pop-tag-mark)       ; Return from whence you came
  )

(use-package rustic
  :ensure t)
