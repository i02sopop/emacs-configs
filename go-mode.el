(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :hook (go-mode . lsp-deferred)
  :config
  (add-hook 'before-save-hook 'gofmt-before-save)
  (define-key go-mode-map (kbd "C-c C-c") 'go-run-buffer)
  (define-key go-mode-map (kbd "C-c C-f") 'gofmt)
  (define-key go-mode-map (kbd "C-c C-d") 'godoc)
  (define-key go-mode-map (kbd "C-c C-a") 'go-import-add)
  (define-key go-mode-map (kbd "C-c C-j") 'lsp-find-definition)
  (define-key go-mode-map (kbd "C-u C-c C-j") 'godef-jump-other-window)
  (define-key go-mode-map (kbd "C-k") 'go-kill)
  (define-key go-mode-map (kbd "M-o") 'go-backward-delete)
  (define-key go-mode-map (kbd "C-c C-r") 'go-remove-unused-imports)
  (define-key go-mode-map (kbd "C-c i") 'go-goto-imports)
  (define-key go-mode-map (kbd "C-c t") 'go-add-tags)
  (define-key go-mode-map (kbd "C-x x") 'go-run)

  (define-key go-mode-map (kbd "C-c C-b") 'pop-tag-mark)       ; Return from whence you came
  (define-key go-mode-map (kbd "M-p") 'compile)            ; Invoke compiler
  (define-key go-mode-map (kbd "M-P") 'recompile)          ; Redo most recent compile cmd
  (define-key go-mode-map (kbd "M-]") 'next-error)         ; Go to next error (or msg)
  (define-key go-mode-map (kbd "M-[") 'previous-error)     ; Go to previous error or msg

  (lsp-register-custom-settings
   '(("gopls.completeUnimported" t t)
	 ("gopls.staticcheck" t t)))
  )

;; Set up before-save hooks to format buffer and add/delete imports.
;; Make sure you don't have other gofmt/goimports hooks enabled.
(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
;;(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)
(add-hook 'go-mode-hook (defun go-init-config ()
						  "Set the init configuration for go"
						  (linum-mode 1)
						  (auto-complete-mode t)
						  (lsp-go-install-save-hooks)
						  ))

(use-package yasnippet
  :ensure t
  :commands yas-minor-mode
  :hook (go-mode . yas-minor-mode))

;; (use-package go-complete
;;   :ensure t
;;   :config
;;   (add-hook 'completion-at-point-functions 'go-complete-at-point))

;; (use-package gotest :ensure t)
;; (use-package go-guru
;;   :ensure t
;;   :config
;;   (go-guru-hl-identifier-mode))

;; (use-package go-eldoc :ensure t)
;; (use-package go-rename :ensure t)
;; (use-package gopher :ensure t)
;; (use-package gotests :ensure t)
;; (use-package go-autocomplete :ensure t)
;; (use-package go-direx :ensure t)
;; (use-package go-errcheck :ensure t)
;; (use-package go-play :ensure t)
;; (use-package go-playground :ensure t)
;; (use-package go-projectile :ensure t)
;; (use-package go-scratch :ensure t)
;; (use-package go-snippets :ensure t)
;; (use-package go-stacktracer :ensure t)
;; (use-package go-dlv :ensure t)
;; (use-package go-gopath :ensure t)
;; (use-package go-impl :ensure t)
;; (use-package go-playground-cli :ensure t)
;; (use-package golint :ensure t)
;; (use-package flymake-go :ensure t)
;; (use-package flycheck-golangci-lint
;;   :ensure t
;;   :hook (go-mode . flycheck-golangci-lint-setup)
;;   :config
;;   (setq flycheck-golangci-lint-config "~/.emacs.d/.golangci.yaml")
;;   )
