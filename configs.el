(require 'package)
(setq package-archives
	  '(("ELPA" . "http://tromey.com/elpa/")
	("gnu" . "http://elpa.gnu.org/packages/")
	("melpa" . "https://melpa.org/packages/")))
;;	("org" . "http://orgmode.org/elpa/")))
;; http://marmalade-repo.org/packages/ is not working, redirects to
;; https://marmalade-repo.org/packages/, which doesn't work

(package-initialize)
(unless package-archive-contents    ;; Refresh the packages descriptions
  (package-refresh-contents))
(setq package-load-list '(all))     ;; List of packages to load

(if (not (package-installed-p 'use-package))
	(progn
	  (package-refresh-contents)
	  (package-install 'use-package)))
(eval-and-compile
  (defvar use-package-verbose t)
  (require 'use-package))

(setq inhibit-startup-message t)
(setq inhibit-startup-screen +1)

(transient-mark-mode +1)
(line-number-mode 't)
(column-number-mode 't)
(display-time)
(auto-compression-mode)

(prefer-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)

(global-set-key "\C-g" 'goto-line)
(global-set-key "\M-i" 'indented-text-mode)
(global-set-key (kbd "C-<") 'mark-previous-like-this)
(global-set-key (kbd "C->") 'mark-next-like-this)
(global-set-key (kbd "C-M-m") 'mark-more-like-this)
(global-set-key (kbd "C-*") 'mark-all-like-this)
(global-set-key "\C-c\C-w" 'backward-kill-word)
(global-set-key "\C-c\C-r" 'kill-region)

(global-set-key [delete] 'delete-char)
(global-set-key [home] 'beginning-of-line)
(global-set-key [end] 'end-of-line)
(global-set-key [C-home] 'beginning-of-buffer)
(global-set-key [C-end] 'end-of-buffer)
(global-set-key [M-up] 'move-text-up)
(global-set-key [M-down] 'move-text-down)

(setq browse-url-browser-function 'browse-url-of-buffer-with-firefox)
(setq global-auto-revert-mode 't)
(setq split-width-threshold 120)
(setq make-backup-files nil)
(setq locale-coding-system 'utf-8)
(setq default-mime-charset 'utf-8)
(setq-default indent-tabs-mode t)
(setq-default tab-width 4)
(setq-default c-basic-offset 4)
(setq-default js-indent-level 2)
(setq-default css-indent-offset 2)
(setq indent-tabs-mode t)
(setq indent-line-function 'insert-tab)
(setq Info-aditional-directory-list '"/usr/local/share/info"
	  auto-image-file-mode 't
	  compile-command '"make"
	  vc-default-init-version '"0.1")

(setq default-major-mode 'text-mode)
(setq initial-major-mode 'text-mode)

(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)

;; Interactively do things.
(ido-mode 1)
(ido-everywhere)
(setq ido-enable-flex-matching t)
(fido-mode)

;; Show stray whitespace.
(setq-default show-trailing-whitespace t)
(setq-default indicate-empty-lines t)
(setq-default indicate-buffer-boundaries 'left)

;; Add a newline automatically at the end of a file while saving.
(setq-default require-final-newline t)

;; Consider a period followed by a single space to be end of sentence.
(setq sentence-end-double-space nil)

;; Highlight matching pairs of parentheses.
(setq show-paren-delay 0)
(show-paren-mode)

(setq custom-file "~/.emacs.d/custom.el")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Postscript configurations ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq ps-paper-type 'a4
	  ps-font-size 10.0
	  ps-print-header nil
	  ps-landscape-mode nil
	  ps-number-of-columns 1
	  ps-top-margin 10
	  ps-left-margin 10
	  ps-right-margin 10
	  ps-inter-column 5
	  ps-font-size 8.25
	  ps-line-number-font-size 10
	  ps-line-number-step 10
	  ps-print-color-p 'black-white
	  ps-number-of-columns 70)

;; Fonts
(add-to-list 'default-frame-alist '(font . "hack"))
(set-face-attribute 'default t :font "hack")

(set-default 'truncate-lines t)
(global-set-key (kbd "\C-ct") 'toggle-truncate-lines)

(load "~/.emacs.d/functions.el")

(global-set-key (kbd "\C-cbs") 'bookmark-set-saved)
(global-set-key (kbd "\C-cbj") 'bookmark-jump-saved)
(global-set-key (kbd "\C-cbd") 'bookmark-delete-saved)

;; Special function key bindings.
(global-set-key (kbd "<f9> b") 'bbdb)
(global-set-key (kbd "<f9> c") 'calendar)
(global-set-key (kbd "<f9> T") 'tabify)
(global-set-key (kbd "<f9> U") 'untabify)
(global-set-key (kbd "<f9> v") 'visible-mode)
(global-set-key (kbd "C-<f9>") 'previous-buffer)
(global-set-key (kbd "C-<f10>") 'next-buffer)
(global-set-key (kbd "M-p") 'previous-buffer)
(global-set-key (kbd "M-n") 'next-buffer)

(require 'server)
(setq server-name
	  (replace-regexp-in-string "\n$" "" (shell-command-to-string "current_project_name")))

(unless (server-running-p (symbol-value 'server-name))
  (server-start))
