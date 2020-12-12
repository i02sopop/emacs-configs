(use-package ac-c-headers
  :ensure t
  :config
  (ac-config-default)
  (define-key ac-completing-map "\C-n" 'ac-next)
  (define-key ac-completing-map "\C-p" 'ac-previous)
  (add-hook 'c-mode-hook
	    (lambda ()
	      (add-to-list 'ac-sources 'ac-source-c-headers)
	      (add-to-list 'ac-sources 'ac-source-c-header-symbols t))))

(use-package ac-capf
  :ensure t
  :config
  (ac-capf-setup))

(use-package ac-etags
  :ensure t
  :config
  (ac-etags-ac-setup)
  (setq ac-etags-requires 4)
  (add-hook 'c-mode-common-hook 'ac-etags-ac-setup))

(use-package ac-html
  :ensure t
  :config
  (ac-html-enable-data-provider 'ac-html-default-data-provider)
  (ac-html-setup)
  (setq ac-sources '(ac-source-html-tag
                     ac-source-html-attr
                     ac-source-html-attrv))
  (auto-complete-mode))

(use-package ac-html-bootstrap
  :ensure t)

(use-package ac-html-csswatcher
  :ensure t
  :config
  (ac-html-csswatcher-setup)
  )

(use-package js2-mode
  :ensure t)

(use-package simple-httpd
  :ensure t
  :config
  (setq httpd-root "/var/www"))
;  (httpd-start))

(use-package skewer-mode
  :ensure t
  :config
  (add-hook 'js2-mode-hook 'skewer-mode)
  (add-hook 'css-mode-hook 'skewer-css-mode)
  (add-hook 'html-mode-hook 'skewer-html-mode))

(use-package js2-refactor
  :ensure t
  :config
  (add-hook 'js2-mode-hook #'js2-refactor-mode))

(use-package ac-js2
  :ensure t
  :config
  (add-hook 'js2-mode-hook 'ac-js2-mode)
  (setq ac-js2-evaluate-calls t)
  ;; (setq ac-js2-external-libraries '("full/path/to/a-library.js"))
  )

(use-package ac-math
  :ensure t
  :config
  (add-to-list 'ac-modes 'latex-mode)
  (defun ac-latex-mode-setup ()
	(setq ac-sources
		  (append '(ac-source-math-unicode ac-source-math-latex ac-source-latex-commands)
				  ac-sources)))
  (add-hook 'TeX-mode-hook 'ac-latex-mode-setup)
  (ac-flyspell-workaround)
  (setq ac-math-unicode-in-math-p t))

(use-package flex-autopair
  :ensure t
  :config
  (flex-autopair-mode 1))

(use-package which-key
  :ensure t
  :config
  (which-key-mode))
