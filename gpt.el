;; -*- lexical-binding: t; -*-

(use-package gpt
  :ensure t)

;; (use-package le-gpt
;;   ;; suggested keybindings
;;   :bind (("M-C-g" . le-gpt-chat)
;;          ("M-C-n" . le-gpt-complete-at-point)
;;          ("M-C-t" . le-gpt-transform-region)
;;          ("M-C-k" . le-gpt-interrupt)
;;          ;; if you use consult
;;          ("C-c C-s" . le-gpt-consult-buffers))
;;   :config
;;   ;; set default values as you wish (and swith with `le-gpt-switch-model`)
;;   (setq le-gpt-api-type 'anthropic)
;;   (setq le-gpt-model "claude-sonnet-4-20250514")
;;   (setq le-gpt-max-tokens 10000)

;;   (setq le-gpt-openai-key "xxx")
;;   (setq le-gpt-anthropic-key "xxx")
;;   (setq le-gpt-deepseek-key "xxx"))
