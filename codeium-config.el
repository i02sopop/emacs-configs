;; -*- lexical-binding: t; -*-

;; Codeium configuration for Emacs
;; Clone the codeium.el repository from https://github.com/Exafunction/codeium.el and set the path accordingly.
;; Install the lsp by calling M-x codeium-install
(load (concat config-dir "/codeium.el/codeium.el"))

;; Company
(use-package company
  :hook (after-init . global-company-mode)
  :custom
  (company-idle-delay 0.1)
  (company-minimum-prefix-length 1)
  (company-tooltip-align-annotations t))

(use-package codeium
    ;; if you use straight
    ;; :straight '(:type git :host github :repo "Exafunction/codeium.el")
    ;; otherwise, make sure that the codeium.el file is on load-path

    :init
    ;; use globally
    (add-to-list 'completion-at-point-functions #'codeium-completion-at-point)
    ;; or on a hook
    ;; (add-hook 'python-mode-hook
    ;;     (lambda ()
    ;;         (setq-local completion-at-point-functions '(codeium-completion-at-point))))

    ;; if you want multiple completion backends, use cape (https://github.com/minad/cape):
    ;; (add-hook 'python-mode-hook
    ;;     (lambda ()
    ;;         (setq-local completion-at-point-functions
    ;;             (list (cape-capf-super #'codeium-completion-at-point #'lsp-completion-at-point)))))
    ;; an async company-backend is coming soon!

    ;; codeium-completion-at-point is autoloaded, but you can
    ;; optionally set a timer, which might speed up things as the
    ;; codeium local language server takes ~0.2s to start up
    ;; (add-hook 'emacs-startup-hook
    ;;  (lambda () (run-with-timer 0.1 nil #'codeium-init)))

    ;; :defer t ;; lazy loading, if you want
    :config
	(setq company-selection-wrap-around t)
    (setq use-dialog-box nil) ;; do not use popup boxes

    ;; if you don't want to use customize to save the api-key
    ;; (setq codeium/metadata/api_key "xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx")

    ;; get codeium status in the modeline
    (setq codeium-mode-line-enable
        (lambda (api) (not (memq api '(CancelRequest Heartbeat AcceptCompletion)))))
    (add-to-list 'mode-line-format '(:eval (car-safe codeium-mode-line)) t)
    ;; alternatively for a more extensive mode-line
    ;; (add-to-list 'mode-line-format '(-50 "" codeium-mode-line) t)

    ;; use M-x codeium-diagnose to see apis/fields that would be sent to the local language server
    (setq codeium-api-enabled
        (lambda (api)
            (memq api '(GetCompletions Heartbeat CancelRequest GetAuthToken RegisterUser auth-redirect AcceptCompletion))))
    ;; you can also set a config for a single buffer like this:
    ;; (add-hook 'python-mode-hook
    ;;     (lambda ()
    ;;         (setq-local codeium/editor_options/tab_size 4)))

    ;; You can overwrite all the codeium configs!
    ;; for example, we recommend limiting the string sent to codeium for better performance
    (defun my-codeium/document/text ()
        (buffer-substring-no-properties (max (- (point) 3000) (point-min)) (min (+ (point) 1000) (point-max))))

    ;; if you change the text, you should also change the cursor_offset
    ;; warning: this is measured by UTF-8 encoded bytes
    (defun my-codeium/document/cursor_offset ()
        (codeium-utf8-byte-length
            (buffer-substring-no-properties (max (- (point) 3000) (point-min)) (point))))

	(setq codeium/document/text 'my-codeium/document/text)
    (setq codeium/document/cursor_offset 'my-codeium/document/cursor_offset)

	(add-hook 'go-mode-hook
			  (lambda ()
				(setq-local completion-at-point-functions '(codeium-completion-at-point))
				(setq-local codeium/editor_options/tab_size 4)
				(setq-local codeium/editor_options/indent_style "space")
				(setq-local codeium/editor_options/indent_size 4)
				(setq-local codeium/editor_options/insert_spaces t)
				(setq-local codeium/editor_options/trim_trailing_whitespace t)
				(setq-local codeium/editor_options/trim_final_newlines t)
				(setq-local codeium/editor_options/ensure_newline_at_eof_on_save t)
				(setq-local codeium/editor_options/format_on_save t)
				(setq-local codeium/editor_options/format_on_save_mode "modifications")
				(setq-local codeium/editor_options/format_on_save_timeout_ms 1000)
				(setq-local codeium/editor_options/prefer_single_line_completion t)
				(setq-local codeium/editor_options/trigger_signature_help_on_typing t)
				(setq-local codeium/editor_options/accept_completion_on_enter "off")
				(setq-local codeium/editor_options/accept_completion_on_tab t)
				(setq-local codeium/editor_options/suggest_selection "recentlyUsedByPrefix")
				(setq-local codeium/editor_options/enable_ghost_text t)
				(setq-local codeium/editor_options/enable_inline_suggestions t)
				(setq-local codeium/editor_options/inline_suggestions_mode "prefix")
				(setq-local codeium/editor_options/enable_prevent_overwrite t)
				(setq-local codeium/editor_options/enable_show_completion_additional_text_edits t)
				(setq-local codeium/editor_options/enable_show_completion_commit_characters t)
				(setq-local codeium/editor_options/enable_show_completion_confidence t)
				(setq-local codeium/editor_options/enable_show_completion_deprecated t)
				(setq-local codeium/editor_options/enable_show_completion_detail t)
				(setq-local codeium/editor_options/enable_show_completion_documentation t)
				(setq-local codeium/editor_options/enable_show_completion_filter_text t)
				(setq-local codeium/editor_options/enable_show_completion_insert_text t)
				(setq-local codeium/editor_options/enable_show_completion_insert_text_rules t)
				(setq-local codeium/editor_options/enable_show_completion_kind t)
				(setq-local codeium/editor_options/enable_show_completion_language t)
				(setq-local codeium/editor_options/enable_show_completion_metadata t)
				(setq-local codeium/editor_options/enable_show_completion_preselect t)
				(setq-local codeium/editor_options/enable_show_completion_range t)
				(setq-local codeium/editor_options/enable_show_completion_reason t)
				(setq-local codeium/editor_options/enable_show_completion_snippet t)
				(setq-local codeium/editor_options/enable_show_completion_sort_text t)
				(setq-local codeium/editor_options/enable_show_completion_source t)
				(setq-local codeium/editor_options/enable_show_completion_tags t)
				(setq-local codeium/editor_options/enable_show_full_signature t)
				(setq-local codeium/editor_options/enable_show_return_type t)
				(setq-local codeium/editor_options/enable_show_signature_help t)
				))
	)
