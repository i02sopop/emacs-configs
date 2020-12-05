(use-package go
  :ensure t)
(use-package go-add-tags
  :ensure t)
(use-package go-autocomplete
  :ensure t)
(use-package go-capf
  :ensure t)
(use-package go-complete
  :ensure t)
(use-package go-direx
  :ensure t)
(use-package go-eldoc
  :ensure t)
(use-package go-errcheck
  :ensure t)
(use-package go-fill-struct
  :ensure t)
(use-package go-gen-test
  :ensure t)
(use-package go-gopath
  :ensure t)
(use-package go-guru
  :ensure t)
(use-package go-imenu
  :ensure t)
(use-package go-imports
  :ensure t)
(use-package go-mode
  :ensure t)
(use-package go-projectile
  :ensure t)
(use-package go-rename
  :ensure t)
(use-package go-snippets
  :ensure t)
(use-package go-stacktracer
  :ensure t)
(use-package go-tag
  :ensure t)
(use-package go-impl
  :ensure t)
(use-package golint
  :ensure t)
(use-package gotest
  :ensure t)
(use-package go-rename
  :ensure t)

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

  (setq lsp-gopls-codelens nil)
  (setq lsp-enable-indentation 't)
  (setq lsp-enable-on-type-formatting nil)
  (setq lsp-before-save-edits 't)
  (setq lsp-enable-completion-at-point 't)
  (setq lsp-server-trace nil)
  (setq gc-cons-threshold 100000000)
  (setq read-process-output-max (* 1024 1024))
  (setq lsp-prefer-capf 't)
  (setq lsp-idle-delay 0.500)
  (setq lsp-print-performance 't)
  (setq gofmt-command "gofumports")

  (lsp-register-custom-settings
   '(("gopls.completeUnimported" t t)
	 ("gopls.staticcheck" t t)))
  )

(use-package which-key
    :config
    (which-key-mode))

;; Set up before-save hooks to format buffer and add/delete imports.
;; Make sure you don't have other gofmt/goimports hooks enabled.
(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))

(add-hook 'go-mode-hook
		  (defun go-init-config ()
			"Set the init configuration for go"
			(linum-mode 1)
			(auto-complete-mode -1)
			(lsp-go-install-save-hooks)))

;; provides fancier overlays.
(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode)

;; Company mode is a standard completion package that works well with lsp-mode.
(use-package company
  :ensure t
  :config
  ;; Optionally enable completion-as-you-type behavior.
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 1))


(use-package yasnippet
  :ensure t
  :commands yas-minor-mode
  :hook (go-mode . yas-minor-mode))
