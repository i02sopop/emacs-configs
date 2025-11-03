;; Env vars

(use-package load-env-vars
  :ensure t)

(defvar ritho-acronis-go-dotenv-file-name "/tmp/_global.env"
  "Go .env file.")

;; (defvar ritho-acronis-python-dotenv-file-name "~/.env"
;;   "Python .env file.")

(defun ritho-set-project-env ()
  "Export all environment variables in the closest .env file."

  (let ((env-file ritho-acronis-go-dotenv-file-name))
    (when env-file
      (load-env-vars env-file)))

  ;; (let ((env-file ritho-acronis-python-dotenv-file-name))
  ;;   (when env-file
  ;;     (load-env-vars env-file)))
  )

(defun ritho-set-env-vars-hooks ()
  (add-hook 'projectile-mode-hook #'ritho-set-project-env)
  (add-hook 'projectile-after-switch-project-hook #'ritho-set-project-env)
  (add-hook 'comint-exec-hook #'ritho-set-project-env)
  (add-hook 'lsp-mode-hook #'ritho-set-project-env)
  (add-hook 'vterm-mode-hook #'ritho-set-project-env)
  )
