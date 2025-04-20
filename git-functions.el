(defun emacs-solo/vc-git-reflog ()
  "Show git reflog in a new buffer with ANSI colors and custom keybindings."
  (interactive)
  (let* ((root (vc-root-dir))
		 (buffer (get-buffer-create "*vc-git-reflog*")))
	(with-current-buffer buffer
	  (setq-local vc-git-reflog-root root)
	  (let ((inhibit-read-only t))
	  (erase-buffer)
	  (vc-git-command buffer nil nil
					  "reflog"
					  "--color=always"
					  "--pretty=format:%C(yellow)%h%Creset %C(auto)%d%Creset %Cgreen%gd%Creset %s %Cblue(%cr)%Creset")
	  (goto-char (point-min))
	  (ansi-color-apply-on-region (point-min) (point-max)))

	(let ((map (make-sparse-keymap)))
	  (define-key map (kbd "/") #'isearch-forward)
	  (define-key map (kbd "p") #'previous-line)
	  (define-key map (kbd "n") #'next-line)
	  (define-key map (kbd "q") #'kill-buffer-and-window)

	  (use-local-map map))

	(setq buffer-read-only t)
	(setq mode-name "Git-Reflog")
	(setq major-mode 'special-mode))
  (pop-to-buffer buffer)))

(global-set-key (kbd "C-x v R") 'emacs-solo/vc-git-reflog)

(defun emacs-solo/vc-browse-remote (&optional current-line)
  "Open the repository's remote URL in the browser.
If CURRENT-LINE is non-nil, point to the current branch, file, and line.
Otherwise, open the repository's main page."
  (interactive "P")
  (let* ((remote-url (string-trim (vc-git--run-command-string nil "config" "--get" "remote.origin.url")))
		 (branch (string-trim (vc-git--run-command-string nil "rev-parse" "--abbrev-ref" "HEAD")))
		 (file (string-trim (file-relative-name (buffer-file-name) (vc-root-dir))))
		 (line (line-number-at-pos)))
	(message "Opening remote on browser: %s" remote-url)
	(if (and remote-url (string-match "\\(?:git@\\|https://\\)\\([^:/]+\\)[:/]\\(.+?\\)\\(?:\\.git\\)?$" remote-url))
		(let ((host (match-string 1 remote-url))
			  (path (match-string 2 remote-url)))
		  ;; Convert SSH URLs to HTTPS (e.g., git@github.com:user/repo.git -> https://github.com/user/repo)
		  (when (string-prefix-p "git@" host)
			(setq host (replace-regexp-in-string "^git@" "" host)))
		  ;; Construct the appropriate URL based on CURRENT-LINE
		  (browse-url
		   (if current-line
			   (format "https://%s/%s/blob/%s/%s#L%d" host path branch file line)
			 (format "https://%s/%s" host path))))
	  (message "Could not determine repository URL"))))

(global-set-key (kbd "C-x v B") 'emacs-solo/vc-browse-remote)

(defun emacs-solo/vc-diff-on-current-hunk ()
  "Show the diff for the current file and jump to the hunk containing the current line."
  (interactive)
  (let ((current-line (line-number-at-pos)))
	(message "Current line in file: %d" current-line)
	(vc-diff) ; Generate the diff buffer
	(with-current-buffer "*vc-diff*"
	  (goto-char (point-min))
	  (let ((found-hunk nil))
		(while (and (not found-hunk)
					(re-search-forward "^@@ -\\([0-9]+\\), *[0-9]+ \\+\\([0-9]+\\), *\\([0-9]+\\) @@" nil t))
		  (let* ((start-line (string-to-number (match-string 2)))
				   (line-count (string-to-number (match-string 3)))
				   (end-line (+ start-line line-count)))
			(message "Found hunk: %d to %d" start-line end-line)
			(when (and (>= current-line start-line)
						 (<= current-line end-line))
			  (message "Current line %d is within hunk range %d to %d" current-line start-line end-line)
			  (setq found-hunk t)
			  (goto-char (match-beginning 0)))))
		(unless found-hunk
		  (message "Current line %d is not within any hunk range." current-line)
		  (goto-char (point-min)))))))

(global-set-key (kbd "C-x v =") 'emacs-solo/vc-diff-on-current-hunk)
