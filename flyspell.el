(use-package ace-flyspell
  :ensure t
  :config
  (ace-flyspell-setup))

(use-package auto-dictionary
  :ensure t
  :config
  (add-hook 'flyspell-mode-hook (lambda () (auto-dictionary-mode 1))))

(use-package flyspell-correct-popup
  :ensure t
  :config
  (define-key flyspell-mode-map (kbd "C-;") #'flyspell-correct-previous-word-generic)

  (setq ispell-program-name (executable-find "hunspell")
      ispell-dictionary "es_ES")

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
