;;; $Id$
(defun english-mode()
  "English Mode"
  (interactive)
  (auto-complete-mode 1)
  (flyspell-prog-mode)
  (add-to-list 'ac-user-dictionary-files "~/emacs/ac-dict/english-mode")
  (ac-clear-dictionary-cache)
  )

(defun spanish-mode()
  "Spanish Mode"
  (interactive)
  (auto-complete-mode 1)
  (flyspell-prog-mode)
  (add-to-list 'ac-user-dictionary-files "~/emacs/ac-dict/spanish-mode")
  (ac-clear-dictionary-cache)
  )

(defun linux-c-mode()
  "C mode con los ajustes por defecto que usa Linus para el kernel."
  (interactive)
  (c-mode)
  (display-line-numbers-mode)
  (auto-complete-mode 1)
  (hs-minor-mode 1)
  (c-set-style "K&R")
  (hs-hide-all)
  (setq c-indent-level 4)
  (setq c-basic-offset 4)
  (setq c-brace-imaginary-offset 0)
  (setq c-brace-offset -4)
  (setq c-argdecl-indent 4)
  (setq c-label-offset -4)
  (setq c-continued-statement-offset 4)
  (setq indent-tabs-mode t)
  (setq tab-width 4))

(defun ritho-c-mode-func()
  "Mis ajustes para el C mode."
  (interactive)
  (c-mode)
  (display-line-numbers-mode)
  (setq show-trailing-whitespace t)
  (auto-complete-mode 1)
  (hs-minor-mode 1)
  (c-set-style "K&R")
  (hs-hide-all)
  (setq c-indent-level 4)
  (setq c-basic-offset 4)
  (setq c-brace-imaginary-offset 0)
  (setq c-brace-offset -4)
  (setq c-argdecl-indent 4)
  (setq c-label-offset -4)
  (setq c-continued-statement-offset 4)
  (setq indent-tabs-mode t)
  (setq tab-width 4))

(c-add-style "ritho-c-mode" '((ritho-c-mode-func)))

(defun ritho-c++-mode-func()
  "Mis ajustes para el C++ mode."
  (interactive)
  (c++-mode)
  (display-line-numbers-mode)
  (setq show-trailing-whitespace t)
  (auto-complete-mode 1)
  (c-set-style "K&R")
  (setq c-indent-level 4)
  (setq c-basic-offset 4)
  (setq c-brace-imaginary-offset 0)
  (setq c-brace-offset -4)
  (setq c-argdecl-indent 4)
  (setq c-label-offset -4)
  (setq c-continued-statement-offset 4)
  (setq indent-tabs-mode t)
  (setq tab-width 4))

(c-add-style "ritho-c++-mode" '((ritho-c++-mode-func)))

(defun ritho-php-mode-func()
  "Mis ajustes para el PHP mode."
  (interactive)
  (display-line-numbers-mode)
  (ac-config-default)
  (yas/global-mode 1)
  (auto-complete-mode 1)
  (setq flymake-phpcs-standard "~/emacs/php-ritho-standard")
  (setq flymake-phpcs-show-rule t)
  (flymake-php-load)
  (flymake-phpcs-load)
  (c-set-style "K&R")
  (setq php-completion-mode 't)
  (setq show-trailing-whitespace t)
  (setq c-indent-level 4)
  (setq c-basic-offset tab-width)
  (setq c-brace-imaginary-offset 0)
  (setq c-brace-offset -4)
  (setq c-argdecl-indent 4)
  (setq c-label-offset -4)
  (setq c-continued-statement-offset 4)
  (setq c-indent-comments-syntactically-p t)
  (setq indent-tabs-mode t)
  (setq fill-column 78)
  (setq tab-width 4)
  (global-set-key "\C-cc" 'php-compile)
  (c-set-offset 'case-label tab-width)
  (c-set-offset 'block-open '-)
  (c-set-offset 'block-close 0)
  (c-set-offset 'c 1)
  (c-set-offset 'string '+)
  (c-set-offset 'statement-cont '+))

(defun ritho-java-mode()
  "Mis ajustes para el java-mode"
  (interactive)
  (java-mode)
  (display-line-numbers-mode)
  (c-set-style "K&R")
  (setq c-indent-level 4)
  (setq c-basic-offset 4)
  (setq c-brace-imaginary-offset 0)
  (setq c-brace-offset -4)
  (setq c-argdecl-indent 4)
  (setq c-label-offset -4)
  (setq c-continued-statement-offset 4)
  (setq indent-tabs-mode t)
  (setq tab-width 4))

(defun ritho-android-mode()
  "Settings for the android-mode"
  (interactive)
  (java-mode)
  (android-mode)
  (display-line-numbers-mode)
  (setq show-trailing-whitespace t)
  (auto-complete-mode 1)
  (setq indent-tabs-mode t)
  (setq fill-column 78)
  (setq tab-width 4)
  (setq c-basic-offset tab-width)
  (c-set-style "K&R")
  (setq c-indent-level 4)
  (setq c-basic-offset 4)
  (setq c-brace-imaginary-offset 0)
  (setq c-brace-offset -4)
  (setq c-argdecl-indent 4)
  (setq c-label-offset -4)
  (setq c-continued-statement-offset 4)
  (setq indent-tabs-mode t)
  (setq tab-width 4))

(define-generic-mode 'xmodmap-mode
  '(?!)
  '("add" "clear" "keycode" "keysym" "pointer" "remove")
  nil
  '("[xX]modmap\\(rc\\)?\\'")
  nil
  "Simple mode for xmodmap files.")

(defun json-to-single-line (beg end)
  "Collapse prettified json in region between BEG and END to a single line"
  (interactive "r")
  (if (use-region-p)
      (save-excursion
        (save-restriction
          (narrow-to-region beg end)
          (goto-char (point-min))
          (while (re-search-forward "[[:space:]\n]+" nil t)
            (replace-match ""))))
    (print "This function operates on a region")))

(defun ritho-lsp-find-definition()
  "Find the definition"
  (interactive)
  (lsp-find-definition)
  (hs-show-block))

(eval-after-load 'hideshow
  '(progn
	 (global-set-key (kbd "<C-tab>") 'hs-toggle-hiding)
	 (global-set-key (kbd "\C-c+") 'hs-show-block)
	 (global-set-key (kbd "\C-c-") 'hs-hide-block)))
