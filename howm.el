(use-package counsel
  :ensure t)

(use-package orgalist
  :ensure t)

(use-package zotero
  :ensure t)

(use-package bibtex-capf
  :ensure t)

(use-package bibtex-completion
  :ensure t)

(use-package howm
  :ensure t
  :init
  (setq howm-prefix "\C-c;")
  :config
  ;; Directory configuration
  (setq howm-home-directory "~/notes/")
  (setq howm-directory "~/notes/")
  (setq howm-keyword-file (expand-file-name ".howm-keys" howm-home-directory))
  (setq howm-history-file (expand-file-name ".howm-history" howm-home-directory))
  (setq howm-file-name-format "%Y/%m/%Y-%m-%d-%H%M%S.org")

  ;; Use ripgrep as grep
  ;; This needs the package ripgrep installed.
  (setq howm-view-use-grep t)
  (setq howm-view-grep-command "rg")
  (setq howm-view-grep-option "-nH --no-heading --color never")
  (setq howm-view-grep-extended-option nil)
  (setq howm-view-grep-fixed-option "-F")
  (setq howm-view-grep-expr-option nil)
  (setq howm-view-grep-file-stdin-option nil)

  (setq howm-menu-refresh-after-save nil)
  (setq howm-menu-expiry-hours 6)  ;; cache menu N hours
  ;; (setq howm-menu-file "0000-00-00-000000.txt")  ;; don't *search*

  ;; counsel-rg for howm
  (defun howm-list--counsel-rg (match)
	(if (string= match "")
		(howm-list-all)
	  (if (or (null ivy--old-cands)
			  (equal ivy--old-cands '("No matches found")))
          (message "No match")
		(let ((howm-view-use-grep
			   #'(lambda (str file-list &optional fixed-p force-case-fold)
                   (mapcar
					(lambda (cand)
					  (if (string-match "\\`\\(.*\\):\\([0-9]+\\):\\(.*\\)\\'" cand)
                          (let ((file (match-string-no-properties 1 cand))
								(line (match-string-no-properties 2 cand))
								(match-line (match-string-no-properties 3 cand)))
							(list (expand-file-name file howm-directory)
                                  (string-to-number line)
                                  match-line))))
					ivy--old-cands))))
          (howm-search ivy--old-re t)
          (riffle-set-place
		   (1+ (cl-position match ivy--old-cands :test 'string=)))))))

  (defun howm-counsel-rg ()
	"Interactively grep for a string in your howm notes using rg."
	(interactive)
	(let ((default-directory howm-directory)
          (counsel-ag-base-command counsel-rg-base-command)
          (counsel-ag-command (counsel--format-ag-command "--glob=!*~" "%s")))
      (ivy-read "Search all (rg): "
				#'counsel-ag-function
				:dynamic-collection t
				:keymap counsel-ag-map
				:action #'howm-list--counsel-rg
				:require-match t
				:caller 'counsel-rg)))

  (define-key global-map (concat howm-prefix "L") 'howm-list-all)
  (define-key global-map (concat howm-prefix "M") 'howm-menu)
  (define-key global-map (concat howm-prefix "N") 'howm-open-named-file)
  (define-key global-map (concat howm-prefix "R") 'howm-refresh)
  (define-key global-map (concat howm-prefix "S") 'howm-counsel-rg)

  ;; Default recent to sorting by mtime
  (advice-add 'howm-list-recent :after #'howm-view-sort-by-mtime)
  ;; Default all to sorting by creation, newest first
  (advice-add 'howm-list-all :after #'(lambda () (howm-view-sort-by-date t)))

  ;; Rename buffers to their title
  (add-hook 'howm-mode-hook 'howm-mode-set-buffer-name)
  (add-hook 'after-save-hook 'howm-mode-set-buffer-name)

  (add-hook 'howm-mode-hook 'orgalist-mode)

  (define-key howm-menu-mode-map "\C-h" nil)
  (define-key riffle-summary-mode-map "\C-h" nil)
  (define-key howm-view-contents-mode-map "\C-h" nil)

  ;; zotero://
  (add-to-list 'action-lock-default-rules
               (list "\\<zotero://\\S +" (lambda (&optional dummy)
                                           (browse-url (match-string-no-properties 0)))))
  ;; @bibtex
  (add-to-list 'action-lock-default-rules
               (list "\\s-\\(@\\([a-zA-Z0-9:-]+\\)\\)\\>"
					 (lambda (&optional dummy)
                       (browse-url (concat "zotero://select/items/bbt:"
                                           (match-string-no-properties 2))))
					 1))

  ;; make wiki-links jump to single title hit if possible
  (add-to-list 'action-lock-default-rules
               (list howm-wiki-regexp
					 (lambda (&optional dummy)
                       (let ((s (match-string-no-properties howm-wiki-regexp-pos)))
						 ;; letting create-p be nil here, howm-keyword-search-subr
						 ;; should check create-p after open-unique-p
						 (howm-keyword-search (concat "= " s) nil t)))
					 howm-wiki-regexp-hilit-pos))
  )
