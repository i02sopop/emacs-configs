(use-package java-snippets :ensure t)
(use-package jtags :ensure t)
(use-package meghanada :ensure t)

(use-package java-imports
  :ensure t
  :config
  ;; whatever you want to bind it to
  (define-key java-mode-map (kbd "M-I") 'java-imports-add-import-dwim)

  ;; See customization below for where to put java imports
  (setq java-imports-find-block-function 'java-imports-find-place-sorted-block)
  (add-hook 'java-mode-hook 'java-imports-scan-file))

(use-package javadoc-lookup
  :ensure t
  :config
  (global-set-key (kbd "C-h j") 'javadoc-lookup)
  (javadoc-add-roots "/usr/share/doc/openjdk-14-jdk/api")
  )

(use-package javap-mode
  :ensure t
  :config
  (add-hook 'javap-mode-hook
			(lambda ()
			  (javarun-mode 1)
			  (meghanada-mode t)
			  (add-hook 'before-save-hook 'meghanada-code-beautify-before-save))))
