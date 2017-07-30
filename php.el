(use-package php-mode
  :ensure t
  :config
  (require 'php-ext)
  (add-hook 'php-mode-hook 'php-enable-default-coding-style))

(use-package php+-mode
  :ensure t
  :config
  (php+-mode-setup))

;; If you use Composer, you might put something like this in ~/project/.dir-locals.el:
;; (php-mode . ((php-auto-yasnippet-required-files (list "~/project/vendor/autoload.php"))))
(use-package php-auto-yasnippets
  :ensure t
  :config
  (define-key php-mode-map (kbd "C-c C-y") 'yas/create-php-snippet)
  (setq php-auto-yasnippet-php-program "~/.emacs.d/elpa/php-auto-yasnippets-20141128.1411/Create-PHP-YASnippet.php"))

(use-package ac-php
  :ensure t
  :config
  (require 'cl)
  (add-hook 'php-mode-hook
            '(lambda ()
               (auto-complete-mode t)
               (require 'ac-php)
               (setq ac-sources  '(ac-source-php ))
               (yas-global-mode 1)
               (define-key php-mode-map (kbd "C-]") 'ac-php-find-symbol-at-point)
               (define-key php-mode-map (kbd "C-t") 'ac-php-location-stack-back)
               ))
  )

(use-package ac-php-core
  :ensure t)

(use-package composer
  :ensure t)

(use-package flymake-php
  :ensure t
  :config
  (add-hook 'php-mode-hook 'flymake-php-load))

(use-package flymake-phpcs
  :ensure t
  :config
  (setq flymake-phpcs-show-rule t)
  ;; (setq flymake-phpcs-standard
  ;; "~/projects/devtools/php_codesniffer/MyCompanyStandard")
  )

(use-package anything
  :ensure t)

(use-package php-completion
  :ensure t)

(use-package php-extras
  :ensure t)

(use-package php-refactor-mode
  :ensure t
  :config
  (add-hook 'php-mode-hook 'php-refactor-mode))

(use-package phpcbf
  :ensure t
  :config
  (setq phpcbf-standard "PSR2")
  (add-hook 'php-mode-hook 'phpcbf-enable-on-save))

(use-package phpunit
  :ensure t
  :config
  (define-key php-mode-map (kbd "C-t t") 'phpunit-current-test)
  (define-key php-mode-map (kbd "C-t c") 'phpunit-current-class)
  (define-key php-mode-map (kbd "C-t p") 'phpunit-current-project)
  (add-to-list 'auto-mode-alist '("\\.php$'" . phpunit-mode))
  (setq phpunit-configuration-file "phpunit.xml")
  (setq phpunit-root-directory "./"))

(use-package psysh
  :ensure t)

(defun ritho-php-mode-func()
  "Mis ajustes para el PHP mode."
  (interactive)
  (linum-mode)
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
