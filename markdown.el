(use-package gh-md :ensure t)
(use-package markdown-mode :ensure t)
(use-package markdown-toc :ensure t)
(use-package markdownfmt :ensure t)

(use-package flymd
  :ensure t
  :config
  (setq flymd-browser-open-function 'browse-url-of-buffer-with-firefox))

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown")
  :bind (:map markdown-mode-map
			  ("C-c C-e" . markdown-do)
			  ("C-c C-f" . markdownfmt-format-buffer))
  :config
  (add-hook 'markdown-mode-hook #'markdownfmt-enable-on-save))
