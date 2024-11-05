(use-package go-add-tags
  :ensure t)
(use-package go-autocomplete
  :ensure t)
(use-package go-complete
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
  :ensure t
  :config
  ;; Install godoctor with git clone https://github.com/godoctor/godoctor
  ;; cd godoctor && go install
  (use-package godoctor
	:ensure t
	:config
	(define-key go-mode-map (kbd "C-x C-g r") 'godoctor-rename)
	(define-key go-mode-map (kbd "C-x C-g e") 'godoctor-extract)
	(define-key go-mode-map (kbd "C-x C-g t") 'godoctor-toggle)
	(define-key go-mode-map (kbd "C-x C-g d") 'godoctor-godoc))

  (add-hook 'go-mode-hook
			(defun go-init-config ()
			  "Set the init configuration for go"
			  (display-line-numbers-mode)
			  (auto-complete-mode -1)
			  (hs-minor-mode 1)
			  (hs-hide-all)))
  )

(use-package go-projectile
  :ensure t)
(use-package go-rename
  :ensure t)
(use-package go-stacktracer
  :ensure t)
(use-package go-impl
  :ensure t)
(use-package go-rename
  :ensure t)
(use-package go-expr-completion
  :ensure t)

(use-package gotest
  :ensure t
  :config
  (define-key go-mode-map (kbd "C-c C-g C-r C-t") 'go-test-current-test)
  (define-key go-mode-map (kbd "C-c C-g C-r C-c") 'go-test-current-coverage)
  (define-key go-mode-map (kbd "C-c C-g C-r C-f") 'go-test-current-file)
  (define-key go-mode-map (kbd "C-c C-g C-r C-b") 'go-test-current-file-benchmarks)
  (define-key go-mode-map (kbd "C-c C-g C-r C-p") 'go-test-current-project)
  (define-key go-mode-map (kbd "C-c C-g C-r C-B") 'go-test-current-project-benchmarks)
  )

(use-package go-tag
  :ensure t
  :config
  (define-key go-mode-map (kbd "C-c C-g C-t C-a") 'go-tag-add)
  (define-key go-mode-map (kbd "C-c C-g C-t C-d") 'go-tag-remove)
  (define-key go-mode-map (kbd "C-c C-g C-t C-r") 'go-tag-refresh)
  )

(use-package golint
  :ensure t
  :config
  (define-key go-mode-map (kbd "C-c C-g C-l") 'golint)
  )

(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :hook ((go-mode . lsp-deferred)
		 (c-mode . lsp-deferred)
		 (lsp-mode . lsp-enable-which-key-integration))
  :config
  (define-key go-mode-map (kbd "C-c C-j") 'ritho-lsp-find-definition)
  (define-key go-mode-map (kbd "C-c C-r") 'lsp-find-references)
  (define-key go-mode-map (kbd "C-c C-b") 'pop-tag-mark)       ; Return from whence you came

  (define-key c-mode-map (kbd "C-c C-j") 'lsp-find-definition)
  (define-key c-mode-map (kbd "C-c C-r") 'lsp-find-references)
  (define-key c-mode-map (kbd "C-c C-b") 'pop-tag-mark)       ; Return from whence you came

  (setq lsp-keymap-prefix "C-c l")
  (define-key lsp-mode-map (kbd "C-c l") lsp-command-map)
  (setq lsp-file-watch-threshold 15000)
  (setq lsp-completion-enable 't)

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
  (setq gofmt-command "gofumpt")

  (lsp-register-custom-settings
   '(("gopls.completeUnimported" t t)
	 ("gopls.completionDocumentation" t t)
	 ("gopls.staticcheck" t t)
	 ("gopls.gofumpt" t t)))

  ;; Install gci with `go install github.com/daixiang0/gci@latest`.
  (defun gci-organize-imports ()
	"Organize the imports with gci."
	(interactive)
	(setq go-project (shell-command-to-string "go list -m"))
	(start-process "gci"
				   (get-buffer-create "*gci-import-buffer*")
				   "gci"
				   "write"
				   "-s" "standard"
				   "-s" "default"
				   "-s" (concat "prefix(" go-project ")")
				   (buffer-file-name))
	(sit-for 0.1)
	(revert-buffer nil 't nil))

  ;; Set up before-save hooks to format buffer and add/delete imports.
  ;; Make sure you don't have other gofmt/goimports hooks enabled.
  (defun lsp-go-install-save-hooks ()
	(add-hook 'before-save-hook #'lsp-format-buffer t t)
	(add-hook 'before-save-hook #'lsp-organize-imports t t)
	(add-hook 'before-save-hook 'gofmt-before-save))

  (define-key go-mode-map (kbd "C-x C-g g") 'gci-organize-imports)
  (add-hook 'before-save-hook 'lsp-go-install-save-hooks))

;; provides fancier overlays.
(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-doc-enable nil)
  (setq lsp-ui-doc-delay 0.5)
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references))

(use-package which-key
    :config
    (which-key-mode))

;; Company mode is a standard completion package that works well with lsp-mode.
(use-package company
  :ensure t
  :config
  ;; Optionally enable completion-as-you-type behavior.
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 1)
  (add-to-list 'company-backends 'company-c-headers))

(use-package yasnippet
  :ensure t
  :commands yas-minor-mode
  :hook (go-mode . yas-minor-mode))

(use-package eglot
  :ensure t
  :config
  (add-to-list 'eglot-server-programs '((c++-mode c-mode) "clangd"))
  (add-hook 'c-mode-hook 'eglot-ensure)
  (add-hook 'c++-mode-hook 'eglot-ensure))

(use-package ccls
  :ensure t
  :hook ((c-mode c++-mode objc-mode cuda-mode) .
         (lambda () (require 'ccls) (lsp)))
  :config
  (setq ccls-executable "/usr/local/bin/ccls")
  (setq ccls-initialization-options
	'(:index (:comments 2) :completion (:detailedLabel t))))

(use-package srefactor
  :ensure t
  :config
  (semantic-mode 1)
  (define-key c-mode-map (kbd "M-RET") 'srefactor-refactor-at-point)
  (define-key c++-mode-map (kbd "M-RET") 'srefactor-refactor-at-point))

(add-to-list 'auto-mode-alist '("\\.h\\'" . ritho-c-mode-func))
(add-to-list 'auto-mode-alist '("\\.c\\'" . ritho-c-mode-func))
