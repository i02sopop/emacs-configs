(defun browse-url-of-buffer-with-firefox (url &optional new-window)
  "Open URL in a new tab in Firefox."
  (interactive (browse-url-interactive-arg "URL: "))
  (let ((cmd (shell-command-to-string
			  (concat "~/firefox/firefox --new-tab " url))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Time functions.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar current-date-time-format "%a %b %d %H:%M:%S %Z %Y"
  "Format of date to insert with `insert-current-date-time' func
See help of `format-time-string' for possible replacements")

(defvar current-time-format "%a %H:%M:%S"
  "Format of date to insert with `insert-current-time' func.
Note the weekly scope of the command's precision.")

(defvar current-timestamp "%H%M%S%d%m%Y"
  "Format of date to insert with `insert-current-timestamp' func.
Note the weekly scope of the command's precision.")

(defun insert-current-date-time ()
  "insert the current date and time into current buffer.
Uses `current-date-time-format' for the formatting the date/time."
  (interactive)
  (insert (format-time-string current-date-time-format (current-time)))
  )

(defun insert-current-time ()
  "insert the current time (1-week scope) into the current buffer."
  (interactive)
  (insert (format-time-string current-time-format (current-time)))
  )

(defun insert-current-time-number ()
  "insert the current time in timestamp format."
  (interactive)
  (insert (format-time-string current-timestamp (current-time)))
  )

(global-set-key "\C-c\C-d" 'insert-current-date-time)
(global-set-key "\C-c\C-t" 'insert-current-time)
(global-set-key "\C-c\C-n" 'insert-current-time-number)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dictionary functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun fd-switch-dictionary()
  (interactive)
  (let* ((dic ispell-current-dictionary)
	 (change (if (string= dic "castellano8") "american" "castellano8")))
    (ispell-change-dictionary change)
    (message "Dictionary switched from %s to %s" dic change)
    ))

;; (global-set-key (kbd "<f8>")   'fd-switch-dictionary)
(global-set-key "\C-x\C-d" 'fd-switch-dictionary)
