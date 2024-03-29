(use-package erlang
  :ensure t
  )

(use-package elixir-mode
  :ensure t
  :custom
  (linum-format "%4d \u2502 ")
  :config
  (setq edts-erl-sname "ritho")
  (add-to-list 'elixir-mode-hook
			   (defun enable-linum-mode ()
				 (display-line-numbers-mode)))
  )

(use-package elixir-mix
  :ensure t)

(use-package elixir-yasnippets
  :ensure t)

(use-package mix
  :ensure t)

(use-package flycheck-credo
  :ensure t)

(use-package flycheck-dialyxir
  :ensure t)

(use-package flycheck-dogma
  :ensure t)

(use-package flycheck-elixir
  :ensure t)

(use-package flymake-elixir
  :ensure t)

(use-package edts
  :ensure t)

(use-package alchemist
  :ensure t
  :config
  (setq alchemist-goto-erlang-source-dir "/usr/lib/erlang")
  (setq alchemist-goto-elixir-source-dir "/usr/local/lib/elixir")

  (define-key alchemist-mode-map (kbd "C-c C-j") 'alchemist-goto-definition-at-point)
  (define-key alchemist-mode-map (kbd "C-c C-b") 'alchemist-goto-jump-back)
  (define-key alchemist-mode-map (kbd "C-c C-c") 'alchemist-mix-compile)
  (define-key alchemist-mode-map (kbd "C-c C-r") 'alchemist-mix-run)
  (define-key alchemist-mode-map (kbd "C-c C-t a") 'alchemist-mix-test)
  (define-key alchemist-mode-map (kbd "C-c C-t l") 'alchemist-mix-rerun-last-test)
  (define-key alchemist-mode-map (kbd "C-c C-t f") 'alchemist-mix-test-file)
  (define-key alchemist-mode-map (kbd "C-c C-t b") 'alchemist-mix-test-this-buffer)
  (define-key alchemist-mode-map (kbd "C-c C-t p") 'alchemist-mix-test-at-point)
  (define-key alchemist-mode-map (kbd "C-c C-p") 'alchemist-phoenix-mode)
  (define-key alchemist-mode-map (kbd "C-c C-h") 'alchemist-help-minor-mode)

  ;; Alchemist phoenix
  (define-key alchemist-mode-map (kbd "C-c a f w") 'alchemist-phoenix-find-web)
  (define-key alchemist-mode-map (kbd "C-c a f c") 'alchemist-phoenix-find-controllers)
  (define-key alchemist-mode-map (kbd "C-c a f C") 'alchemist-phoenix-find-channels)
  (define-key alchemist-mode-map (kbd "C-c a f t") 'alchemist-phoenix-find-templates)
  (define-key alchemist-mode-map (kbd "C-c a f m") 'alchemist-phoenix-find-models)
  (define-key alchemist-mode-map (kbd "C-c a f v") 'alchemist-phoenix-find-views)
  (define-key alchemist-mode-map (kbd "C-c a f s") 'alchemist-phoenix-find-static)
  (define-key alchemist-mode-map (kbd "C-c a f r") 'alchemist-phoenix-router)
  (define-key alchemist-mode-map (kbd "C-c a f R") 'alchemist-phoenix-routes)

  (define-key erlang-mode-map (kbd "M-j") 'alchemist-goto-definition-at-point)
  (define-key erlang-mode-map (kbd "M-b") 'alchemist-goto-jump-back)
  )

(use-package ac-alchemist
  :ensure t)
