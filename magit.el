(use-package magit
  :ensure t
  :config
  (global-set-key (kbd "C-c g") 'magit-status)
  )

;; (use-package magit-delta
;;   :ensure t
;;   :hook (magit-mode . magit-delta-mode))

(use-package magit-org-todos
  :ensure t)

(custom-set-variables
 '(ediff-diff-options "-w")
 '(ediff-split-window-function (quote split-window-horizontally))
 '(ediff-window-setup-function (quote ediff-setup-windows-plain)))
