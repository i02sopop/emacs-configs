(use-package ace-flyspell
  :ensure t
  :config
  (ace-flyspell-setup))

(use-package auto-dictionary
  :ensure t
  :config
  (add-hook 'flyspell-mode-hook (lambda () (auto-dictionary-mode 0))))

(use-package flyspell-correct-popup
  :ensure t
  :config
  (define-key flyspell-mode-map (kbd "C-;") #'flyspell-correct-previous-word-generic)

  (setq ispell-program-name (executable-find "hunspell")
		ispell-dictionary "es_ES")

  (setq ispell-local-dictionary "es_ES")
  (setq ispell-local-dictionary-alist
		;; Please note the list `("-d" "en_US")` contains ACTUAL parameters passed to hunspell
		;; You could use `("-d" "en_US,en_US-med")` to check with multiple dictionaries
		'(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_US") nil utf-8)
		  ("es_ES" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "es_ES") nil utf-8)))

  (bind-key "C-c S"
			(lambda ()
			  (interactive)
			  (ispell-change-dictionary "es_ES")
			  (flyspell-buffer)))

  (bind-key "C-c E"
			(lambda ()
			  (interactive)
			  (ispell-change-dictionary "en_US")
			  (flyspell-buffer)))

  (add-hook 'flyspell-mode-hook #'flyspell-correct-auto-mode))
