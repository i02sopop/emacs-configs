(use-package exwm
  :ensure t
  :config
  (setq exwm-workspace-number 4)
  ;; Make class name the buffer name.
  (add-hook 'exwm-update-class-hook
			(lambda () (exwm-workspace-rename-buffer exwm-class-name)))
  ;; Global keybindings.
  (setq exwm-input-global-keys
		`(([?\s-r] . exwm-reset) ;; s-r: Reset (to line-mode).
          ([?\s-w] . exwm-workspace-switch) ;; s-w: Switch workspace.
          ([?\s-&] . (lambda (cmd) ;; s-&: Launch application.
                       (interactive (list (read-shell-command "$ ")))
                       (start-process-shell-command cmd nil cmd)))
          ;; s-N: Switch to certain workspace.
          ,@(mapcar (lambda (i)
                      `(,(kbd (format "s-%d" i)) .
						(lambda ()
                          (interactive)
                          (exwm-workspace-switch-create ,i))))
					(number-sequence 0 9))))
  ;; Enable EXWM
  (exwm-enable)
  )

(use-package exwm-edit
  :ensure t
  :config
  )

(use-package exwm-firefox-core
  :ensure t
  :config
  )

(use-package exwm-firefox-evil
  :ensure t
  :config
  )

(use-package exwm-float
  :ensure t
  :config
  )

(use-package exwm-mff
  :ensure t
  :config
  )

(use-package exwm-modeline
  :ensure t
  :config
  )

(use-package exwm-surf
  :ensure t
  :config
  )

(use-package exwm-x
  :ensure t
  :config
  )

(use-package helm-exwm
  :ensure t
  :config
  )

(use-package ednc
  :ensure t
  :config
  )
