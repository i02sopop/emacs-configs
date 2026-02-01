;; -*- lexical-binding: t; -*-

(use-package copilot
  :ensure t
  :vc (:url "https://github.com/copilot-emacs/copilot.el"
            :rev :newest
            :branch "main")
  :hook (prog-mode . copilot-mode)
  :bind (:map copilot-completion-map
              ("C-<return>" . 'copilot-accept-completion)
              ("C-n" . 'copilot-next-completion)
              ("C-p" . 'copilot-previous-completion))  :config
  ;; (setq copilot-lsp-settings '(:github-enterprise (:uri "https://github.com")))
  (add-to-list 'copilot-major-mode-alist '("enh-go" . "go"))
  (add-to-list 'copilot-major-mode-alist '("enh-python" . "python"))
  (add-to-list 'copilot-major-mode-alist '("enh-rust" . "rust"))
  (add-to-list 'copilot-major-mode-alist '("enh-javascript" . "javascript"))
  (add-to-list 'copilot-major-mode-alist '("enh-typescript" . "typescript"))
  (add-to-list 'copilot-major-mode-alist '("enh-lisp" . "lisp"))

  (add-to-list 'copilot-indentation-alist '(c-mode 4))
  (add-to-list 'copilot-indentation-alist '(closure-mode 2))
  (add-to-list 'copilot-indentation-alist '(emacs-lisp-mode 2))
  (add-to-list 'copilot-indentation-alist '(go-mode 4))
  (add-to-list 'copilot-indentation-alist '(java-mode 4))
  (add-to-list 'copilot-indentation-alist '(org-mode 2))
  (add-to-list 'copilot-indentation-alist '(prog-mode 2))
  (add-to-list 'copilot-indentation-alist '(python-mode 4))
  (add-to-list 'copilot-indentation-alist '(rst-mode 4))
  (add-to-list 'copilot-indentation-alist '(text-mode 2))
  (add-to-list 'copilot-indentation-alist '(ts-mode 4))

  (defun rk/no-copilot-mode ()
	"Helper for `rk/no-copilot-modes'."
	(copilot-mode -1))

  (defvar rk/no-copilot-modes '(debugger-mode
								fundamental-mode)
	"List of modes where `copilot-mode' should be disabled.")

  (defun rk/copilot-disable-predicate ()
	"When copilot should not automatically show completions."
	(or rk/copilot-manual-mode
		(member major-mode rk/no-copilot-modes)
		(company--active-p)))

  (add-to-list 'copilot-disable-predicates #'rk/copilot-disable-predicate)

  (defvar rk/copilot-manual-mode nil
	"When `t' will only show completions when manually triggered, e.g. via M-C-<return>.")

  (defun rk/copilot-change-activation ()
	"Switch between three activation modes:
- automatic: copilot will automatically overlay completions
- manual: you need to press a key (M-C-<return>) to trigger completions
- off: copilot is completely disabled."
	(interactive)
	(if (and copilot-mode rk/copilot-manual-mode)
		(progn
          (message "deactivating copilot")
          (global-copilot-mode -1)
          (setq rk/copilot-manual-mode nil))
      (if copilot-mode
          (progn
			(message "activating copilot manual mode")
			(setq rk/copilot-manual-mode t))
		(message "activating copilot mode")
		(global-copilot-mode))))

  (define-key global-map (kbd "M-C-<escape>") #'rk/copilot-change-activation)
  )
