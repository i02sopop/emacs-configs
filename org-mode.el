;;;;;;;;;;;;;;
;; org-mode ;;
;;;;;;;;;;;;;;

(use-package org
  :ensure t
  :config
  (require 'org-mu4e)
  (add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\|txt\\)$" . org-mode))

  (require 'org-crypt)

  ;; Encrypt all entries before saving
  (org-crypt-use-before-save-magic)
  (setq org-tags-exclude-from-inheritance (quote ("crypt")))

  ;; GPG key to use for encryption
  (setq org-crypt-disable-auto-save nil))

(use-package org-agenda-property
  :ensure t)

(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(use-package org-context
  :ensure t)

(use-package org-ehtml
  :ensure t)

(use-package org-elisp-help
  :ensure t)

(use-package org-email
  :ensure t)

(use-package org-fstree
  :ensure t)

(use-package org-jekyll
  :ensure t)

(use-package org-jira
  :ensure t)

(use-package org-journal
  :ensure t
  :config
  (setq org-journal-dir "~/journal/")
  (setq org-journal-enable-encryption nil))

(use-package org-magit
  :ensure t)

(use-package org-mime
  :ensure t)

(use-package org-pomodoro
  :ensure t
  :config
  (setq org-pomodoro-keep-killed-pomodoro-time t)
  (setq org-pomodoro-length 50)
  (setq org-pomodoro-start-sound-p t))

(use-package org-present
  :ensure t)

(use-package org-presie
  :ensure t)

(use-package org-table-comment
  :ensure t)

(use-package org-trello
  :ensure t
  :config
  (setq org-trello-current-prefix-keybinding "C-c t")
;;  (setq org-trello-files '("~/org/agenda/adlisting-trello.org"))
  )

(use-package org2blog
  :ensure t
  :config
  (require 'org2blog-autoloads)
  (setq org2blog/wp-blog-alist
		'(("fsfe"
		   :url "https://blogs.fsfe.org/palvarez/xmlrpc.php"
		   :username "palvarez"
		   :tags-as-categories nil)))

  (setq org2blog/wp-track-posts nil)
  (setq org2blog/wp-use-sourcecode-shortcode t)
  (setq org2blog/wp-show-post-in-browser nil)
  (setq org2blog/wp-sourcecode-langs (list "actionscript3"
										   "bash"
										   "coldfusion"
										   "c"
										   "cpp"
										   "csharp"
										   "css"
										   "delphi"
										   "erlang" "fsharp"
										   "diff"
										   "groovy"
										   "javascript"
										   "java"
										   "javafx"
										   "matlab"
										   "objc"
										   "perl"
										   "php"
										   "text"
										   "powershell"
										   "python"
										   "ruby"
										   "scala"
										   "sql"
										   "vb"
										   "xml")))

(use-package orglink
  :ensure t)

(use-package orglue
  :ensure t)

(use-package org-page
  :ensure t)

(use-package org-octopress
  :ensure t
  :config
  (setq org-octopress-directory-top "~/desarrollo/github/ritho/blog")
  (setq org-octopress-directory-posts "~/desarrollo/github/ritho/blog/_posts")
  (setq org-octopress-directory-org-top "~/org")
  (setq org-octopress-directory-org-posts "~/org/blog")
  (setq org-octopress-setup-file "~/.emacs.d/setupfile.org"))

(use-package outorg
  :ensure t)

(use-package bbdb
  :ensure t)

(use-package bbdb-ext
  :ensure t)

(use-package bbdb2erc
  :ensure t)

(use-package bpe
  :ensure t)

(use-package eldoro
  :ensure t)

(use-package epresent
  :ensure t)

(use-package org-sync
  :ensure t)

(use-package org-review
  :ensure t)

(use-package org-wc
  :ensure t)

(setq global-auto-revert-mode t)
(setq require-final-newline t)
(setq split-width-threshold 120)

;; Custom Key Bindings
(global-set-key (kbd "\C-cl") 'org-store-link)
(global-set-key (kbd "\C-ca") 'org-agenda)
(global-set-key (kbd "\C-cb") 'org-iswitchb)
(global-set-key (kbd "C-M-r") 'org-capture)
(global-set-key (kbd "C-c r") 'org-capture)
(global-set-key (kbd "<f12>") 'org-agenda)
(global-set-key (kbd "<f5>") 'bh/org-todo)
(global-set-key (kbd "<S-f5>") 'bh/widen)
(global-set-key (kbd "<C-f6>") '(lambda () (interactive) (bookmark-set "SAVED")))
(global-set-key (kbd "<f6>") '(lambda () (interactive) (bookmark-jump "SAVED")))
(global-set-key (kbd "<f7>") 'bh/set-truncate-lines)
(global-set-key (kbd "<f8>") 'org-cycle-agenda-files)
(global-set-key (kbd "<f9> <f9>") 'bh/show-org-agenda)
(global-set-key (kbd "<f9> a") 'org-archive-subtree)
(global-set-key (kbd "<f9> b") 'bbdb)
(global-set-key (kbd "<f9> c") 'calendar)
(global-set-key (kbd "<f9> f") 'boxquote-insert-file)
(global-set-key (kbd "<f9> g") 'gnus)
(global-set-key (kbd "<f9> h") 'bh/hide-other)
(global-set-key (kbd "<f9> n") 'org-narrow-to-subtree)
(global-set-key (kbd "<f9> w") 'widen)
(global-set-key (kbd "<f9> u") 'bh/narrow-up-one-level)
(global-set-key (kbd "<f9> I") 'bh/punch-in)
(global-set-key (kbd "<f9> O") 'bh/punch-out)
(global-set-key (kbd "<f9> o") 'bh/make-org-scratch)
(global-set-key (kbd "<f9> p") 'bh/phone-call)
(global-set-key (kbd "<f9> P") 'org-pomodoro)
(global-set-key (kbd "<f9> r") 'boxquote-region)
(global-set-key (kbd "<f9> s") 'bh/switch-to-scratch)
(global-set-key (kbd "<f9> t") 'bh/insert-inactive-timestamp)
(global-set-key (kbd "<f9> T") 'tabify)
(global-set-key (kbd "<f9> U") 'untabify)
(global-set-key (kbd "<f9> v") 'visible-mode)
(global-set-key (kbd "<f9> SPC") 'bh/clock-in-last-task)
(global-set-key (kbd "C-<f9>") 'previous-buffer)
(global-set-key (kbd "M-<f9>") 'org-toggle-inline-images)
(global-set-key (kbd "C-x n r") 'narrow-to-region)
(global-set-key (kbd "C-<f10>") 'next-buffer)
(global-set-key (kbd "<f11>") 'org-clock-goto)
(global-set-key (kbd "C-<f11>") 'org-clock-in)
(global-set-key (kbd "C-s-<f12>") 'bh/save-then-publish)
(global-set-key (kbd "M-p") 'previous-buffer)
(global-set-key (kbd "M-n") 'next-buffer)

;; Set variables
(setq remember-annotation-functions (quote (org-remember-annotation)))
(setq remember-handler-functions (quote (org-remember-handler)))
(setq org-directory "~/org/agenda")
(setq org-default-notes-file "~/org/notes.org")
(setq org-use-fast-todo-selection t)
(setq org-treat-S-cursor-todo-selection-as-state-change nil)
(setq org-deadline-warning-days 30)
(setq org-enforce-todo-dependencies t)
(setq org-link-mailto-program (quote (compose-mail "%a" "%s")))
(setq org-reverse-note-order t)
(setq org-startup-indented t)
(setq org-log-done (quote time))
(setq org-log-into-drawer "LOGBOOK")
(setq org-done-keywords (list "DONE" "FINISHED"))
(setq org-habit-show-all-today nil)

;; Agenda settings
(setq org-agenda-include-diary t)
(setq org-agenda-diary-file "~/org/agenda/diary.org")
(setq org-agenda-insert-diary-extract-time t)
(setq org-agenda-repeating-timestamp-show-all t)
(setq org-agenda-show-all-dates t)
(setq org-agenda-skip-deadline-if-done t)
(setq org-agenda-skip-scheduled-if-done t)
(setq org-agenda-start-on-weekday 1)
(setq org-agenda-text-search-extra-files (quote (agenda-archives)))
(setq org-agenda-window-setup (quote current-window))
(setq org-agenda-files (quote ("~/org/agenda")))

(setq org-todo-keywords
      (quote ((sequence "PROJECT(P)" "FINISHED(F)")
	      (sequence "TODO(t)" "NEXT(n)" "STARTED(s)" "DONE(d)")
	      (sequence "WAITING(w)" "DELEGATED(l)" "HOLD(h)" "DEFERRED(f)" "CANCELLED(c)" "PHONE(p)"))))

(setq org-todo-keyword-faces
      (quote (("TODO" :foreground "red" :weight bold)
	      ("NEXT" :foreground "blue" :weight bold)
	      ("STARTED" :foreground "yellow" :weight bold)
	      ("DONE" :foreground "green" :weight bold)
	      ("WAITING" :foreground "orange" :weight bold)
	      ("DEFERRED" :foreground "magenta" :weight bold)
	      ("HOLD" :foreground "orange" :weight bold)
	      ("DELEGATED" :foreground "green" :weight bold)
	      ("CANCELLED" :foreground "green" :weight bold)
	      ("PROJECT" :foreground "yellow" :weight bold)
	      ("FINISHED" :foreground "green" :weight bold)
	      ("PHONE" :foreground "green" :weight bold))))

(setq org-todo-state-tags-triggers
      (quote (("CANCELLED" ("CANCELLED" . t))
	      ("WAITING" ("WAITING" . t))
	      ("HOLD" ("WAITING" . t) ("HOLD" . t))
	      (done ("WAITING") ("HOLD"))
	      ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
	      ("STARTED" ("WAITING") ("CANCELLED") ("HOLD"))
	      ("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
	      ("DONE" ("WAITING") ("CANCELLED") ("HOLD")))))

;; Capture templates for: TODO tasks, Notes, appointments, phone calls, and org-protocol
(setq org-capture-templates
      (quote (("t" "todo" entry (file "~/org/agenda/refile.org")
	       "* TODO %?\n:PROPERTIES:\n:ORDERED: t\n:LOGGING: TODO(!) NEXT(!) STARTED(!) WAITING(!) DELEGATED(!) HOLD(!) DONE(!) DEFERRED(!) CANCELLED(!) PHONE(!) PROJECT(!) FINISHED(!)\n:DESCRIPTION: -\n:ASSIGNED: ritho\n:DIFICULTY: -\n:CREATION_DATE: %U\n:NOTES: -\n:END:\n%a\n" :clock-in t :clock-resume t)
	      ("r" "respond" entry (file "~/org/agenda/refile.org")
	       "* TODO Respond to %:from on %:subject\n:PROPERTIES:\n:ORDERED: t\n:LOGGING: TODO(!) NEXT(!) STARTED(!) WAITING(!) DELEGATED(!) HOLD(!) DONE(!) DEFERRED(!) CANCELLED(!) PHONE(!) PROJECT(!) FINISHED(!)\n:DESCRIPTION: -\nASSIGNED: ritho\n:DIFICULTY: Easy\n:CREATION_DATE: %U\n:NOTES: -\n:END:\n%a\n" :clock-in t :clock-resume t :immediate-finish t)
	      ("n" "note" entry (file "~/org/notes.org")
	       "* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t)
	      ("j" "Journal" entry (file+datetree "~/org/agenda/diary.org")
	       "* %?\n%U\n" :clock-in t :clock-resume t)
	      ("w" "org-protocol" entry (file "~/org/agenda/refile.org")
	       "* TODO Review %c\n%U\n" :immediate-finish t)
	      ("p" "Phone call" entry (file "~/org/agenda/refile.org")
	       "* PHONE %? :PHONE:\n%U" :clock-in t :clock-resume t)
	      ("h" "Habit" entry (file "~/org/agenda/habits.org")
	       "* NEXT %?\n%U\n%a\nSCHEDULED: %t .+1d\n:PROPERTIES:\n:ORDERED: t\n:LOGGING: TODO(!) NEXT(!) STARTED(!) WAITING(!) DELEGATED(!) HOLD(!) DONE(!) DEFERRED(!) CANCELLED(!) PHONE(!) PROJECT(!) FINISHED(!)\n:DESCRIPTION: -\n:ASSIGNED: ritho\n:CREATION_DATE: %U\n:NOTES: -\n:STYLE: habits\n:REPEAT_TO_STATE: NEXT\n:END:\n"))))

;; Calendar
;; (european-calendar)
(setq calendar-week-start-day 1
      calendar-day-name-array
      ["Domingo" "Lunes" "Martes"
       "Miercoles" "Jueves" "Viernes" "Sábado"]
      calendar-month-name-array
      ["Enero" "Febrero" "Marzo" "Abril"
       "Mayo" "Junio" "Julio" "Agosto" "Septiembre"
       "Octubre" "Noviembre" "Diciembre"])

;; Fancy display
(setq view-diary-entries-initially t
      number-of-diary-entries 7)

;; Targets include this file and any file contributing to the agenda - up to 9 levels deep
(setq org-refile-targets (quote ((nil :maxlevel . 9)
				 (org-agenda-files :maxlevel . 9))))

;; Use full outline paths for refile targets - we file directly with IDO
(setq org-refile-use-outline-path t)

;; Targets complete directly with IDO
(setq org-outline-path-complete-in-steps nil)

;; Allow refile to create parent tasks with confirmation
(setq org-refile-allow-creating-parent-nodes (quote confirm))

;; Use IDO for both buffer and file completion and ido-everywhere to t
(setq org-completion-use-ido t)
(setq ido-everywhere t)
(setq ido-max-directory-size 100000)
(ido-mode (quote both))

;;;; Refile settings
(setq org-refile-target-verify-function 'bh/verify-refile-target)

;; Do not dim blocked tasks
(setq org-agenda-dim-blocked-tasks nil)

;; Compact the block agenda view
(setq org-agenda-compact-blocks t)
(setq org-agenda-auto-exclude-function 'bh/org-auto-exclude-function)

;; Resume clocking task when emacs is restarted
(org-clock-persistence-insinuate)

;; Add the appointments to the effort column
(setq org-agenda-columns-add-appointments-to-effort-sum t)

;; Default appointment duration
(setq org-agenda-default-appointment-duration 60)

;; Show lot sof clocking history so it's easy to pick items off the C-F11 list
(setq org-clock-history-length 36)

;; Resume clocking task on clock-in if the clock is open
(setq org-clock-in-resume t)

;; Change tasks to NEXT when clocking in
(setq org-clock-in-switch-to-state 'bh/clock-in-to-started)

;; Separate drawers for clocking and logs
(setq org-drawers (quote ("PROPERTIES" "LOGBOOK")))

;; Save clock data and state changes and notes in the LOGBOOK drawer
(setq org-clock-into-drawer t)

;; Sometimes I change tasks I'm clocking quickly - this removes clocked tasks with 0:00 duration
(setq org-clock-out-remove-zero-time-clocks t)

;; Clock out when moving task to a done state
(setq org-clock-out-when-done t)

;; Save the running clock and all clock history when exiting Emacs, load it on startup
(setq org-clock-persist t)

;; Do not prompt to resume an active clock
(setq org-clock-persist-query-resume nil)

;; Enable auto clock resolution for finding open clocks
(setq org-clock-auto-clock-resolution (quote when-no-clock-is-running))

;; Agenda log mode items to display (closed and state changes by default)
(setq org-agenda-log-mode-items (quote (state)))

;; Include current clocking task in clock reports
(setq org-clock-report-include-clocking-task t)
(setq bh/keep-clock-running nil)
(setq org-clock-sound "~/notify.wav")

;; Allow setting single tags without the menu
(setq org-fast-tag-selection-single-key (quote expert))

;; Use the current window for C-c ' source editing
(setq org-src-window-setup 'current-window)

;; Keep tasks with dates on the global todo lists
(setq org-agenda-todo-ignore-with-date nil)

;; Keep tasks with deadlines on the global todo lists
(setq org-agenda-todo-ignore-deadlines nil)

;; Keep tasks with scheduled dates on the global todo lists
(setq org-agenda-todo-ignore-scheduled nil)

;; Keep tasks with timestamps on the global todo lists
(setq org-agenda-todo-ignore-timestamp nil)

;; Remove completed items from search results
(setq org-agenda-skip-timestamp-if-done t)

;; Display tags farther right
(setq org-agenda-tags-column -102)

;; position the habit graph on the agenda to the right of the default
(setq org-habit-graph-column 50)

;; For tag searches ignore tasks with scheduled and deadline dates
(setq org-agenda-tags-todo-honor-ignore-options t)
(setq org-agenda-span 'day)
(setq org-stuck-projects (quote ("" nil nil "")))
(setq org-archive-mark-done nil)
(setq org-archive-location "%s_archive::* Archived Tasks")
(setq org-show-entry-below (quote ((default))))
(setq org-agenda-cmp-user-defined 'bh/agenda-sort)
(setq org-hide-leading-stars nil)
(setq org-cycle-separator-lines 2)
(setq org-insert-heading-respect-content nil)
(setq org-show-following-heading t)
(setq org-show-hierarchy-above t)
(setq org-show-siblings (quote ((default))))
(setq org-special-ctrl-a/e 'reversed)
(setq org-special-ctrl-k t)
(setq org-yank-adjusted-subtrees t)
(setq org-id-method (quote uuidgen))
(setq org-table-export-default-format "orgtbl-to-csv")
(setq org-use-speed-commands t)
(setq org-export-with-timestamps nil)
(setq org-return-follows-link t)
(setq org-remove-highlights-with-change nil)
(setq org-read-date-prefer-future nil)
(setq org-tags-match-list-sublevels t)
(setq org-agenda-persistent-filter t)
(setq org-agenda-skip-additional-timestamps-same-entry t)
(setq org-table-use-standard-references (quote from))
(setq org-clone-delete-id t)
(setq org-cycle-include-plain-lists t)
(setq org-src-fontify-natively t)
(setq org-startup-folded 'content)
(setq org-enable-priority-commands t)
(setq org-use-sub-superscripts t)
(setq org-src-preserve-indentation t)
(setq org-agenda-view-columns-initially t)
(setq org-agenda-start-with-clockreport-mode t)

;; convert org mode to HTML automatically
(setq org-mu4e-convert-to-html t)

;; Agenda clock report parameters
(setq org-agenda-clockreport-parameter-plist
      (quote (:link t :maxlevel 5 :fileskip0 t :compact t :narrow 80)))

;; Set default column view headings: Task Effort Clock_Summary
(setq org-columns-default-format
      "%80ITEM(Task) %20CATEGORY %10TODO %10PRIORITY %10Effort(Effort){:} %10CLOCKSUM")

;; global Effort estimate values
;; global STYLE property values for completion
(setq org-global-properties (quote (("Effort_ALL" . "0:15 0:30 0:45 1:00 2:00 3:00 4:00 5:00 6:00 0:00")
				    ("STYLE_ALL" . "habit"))))

(setq org-agenda-time-grid (quote ((daily today require-timed remove-match)
								   (0000 0100 0200 0300 0400 0500 0600 0700 0800 0900 1000 1100 1200 1300 1400 1500 1600 1700 1800 1900 2000 2100 2200 2300)
								   "......"
								   "----------------")))

(setq org-blank-before-new-entry (quote ((heading)
					 (plain-list-item . auto))))

(setq org-link-frame-setup (quote ((vm . vm-visit-folder)
				   (gnus . org-gnus-no-new-news)
				   (file . find-file))))

(setq org-list-demote-modify-bullet (quote (("+" . "-")
					    ("*" . "-")
					    ("1." . "-")
					    ("1)" . "-"))))

(setq org-file-apps (quote ((auto-mode . emacs)
			    ("\\.mm\\'" . system)
			    ("\\.x?html?\\'" . system)
			    ("\\.pdf\\'" . system))))

(setq org-agenda-clock-consistency-checks
      (quote (:max-duration "4:00"
			    :min-duration 0
			    :max-gap 0
			    :gap-ok-around ("4:00"))))

(setq org-structure-template-alist
      (quote (("s" "#+begin_src ?\n\n#+end_src" "<src lang=\"?\">\n\n</src>")
	      ("e" "#+begin_example\n?\n#+end_example" "<example>\n?\n</example>")
	      ("q" "#+begin_quote\n?\n#+end_quote" "<quote>\n?\n</quote>")
	      ("v" "#+begin_verse\n?\n#+end_verse" "<verse>\n?\n/verse>")
	      ("c" "#+begin_center\n?\n#+end_center" "<center>\n?\n/center>")
	      ("l" "#+begin_latex\n?\n#+end_latex" "<literal style=\"latex\">\n?\n</literal>")
	      ("L" "#+latex: " "<literal style=\"latex\">?</literal>")
	      ("h" "#+begin_html\n?\n#+end_html" "<literal style=\"html\">\n?\n</literal>")
	      ("H" "#+html: " "<literal style=\"html\">?</literal>")
	      ("a" "#+begin_ascii\n?\n#+end_ascii")
	      ("A" "#+ascii: ")
	      ("i" "#+index: ?" "#+index: ?")
	      ("I" "#+include %file ?" "<include file=%file markup=\"?\">"))))

;; Enable habit tracking (and a bunch of other modules)
(setq org-modules (quote (org-bbdb
			  org-bibtex
			  org-crypt
			  org-ctags
			  org-gnus
			  org-habit
			  org-id
			  org-info
			  org-irc
			  org-checklist
			  org-choose
			  org-git-link
			  org-man
			  org-notmuch
			  org-panel
			  org-secretary
			  org-toc
			  org-velocity
			  org-jsinfo
			  org-habit
			  org-inlinetask
			  org-irc
			  org-mew
			  org-mhe
			  org-protocol
			  org-rmail
			  org-vm
			  org-wl
			  org-w3m)))

(setq org-speed-commands-user (quote (("0" . ignore)
				      ("1" . ignore)
				      ("2" . ignore)
				      ("3" . ignore)
				      ("4" . ignore)
				      ("5" . ignore)
				      ("6" . ignore)
				      ("7" . ignore)
				      ("8" . ignore)
				      ("9" . ignore)
				      ("a" . ignore)
				      ("d" . ignore)
				      ("h" . bh/hide-other)
				      ("i" progn
				       (forward-char 1)
				       (call-interactively 'org-insert-heading-respect-content))
				      ("k" . org-kill-note-or-show-branches)
				      ("l" . ignore)
				      ("m" . ignore)
				      ("q" . bh/show-org-agenda)
				      ("r" . ignore)
				      ("s" . org-save-all-org-buffers)
				      ("w" . org-refile)
				      ("x" . ignore)
				      ("y" . ignore)
				      ("z" . org-add-note)
				      ("A" . ignore)
				      ("B" . ignore)
				      ("E" . ignore)
				      ("F" . bh/restrict-to-file-or-follow)
				      ("G" . ignore)
				      ("H" . ignore)
				      ("J" . org-clock-goto)
				      ("K" . ignore)
				      ("L" . ignore)
				      ("M" . ignore)
				      ("N" . bh/narrow-to-subtree)
				      ("P" . bh/narrow-to-project)
				      ("Q" . ignore)
				      ("R" . ignore)
				      ("S" . ignore)
				      ("T" . bh/org-todo)
				      ("U" . bh/narrow-up-one-level)
				      ("V" . ignore)
				      ("W" . bh/widen)
				      ("X" . ignore)
				      ("Y" . ignore)
				      ("Z" . ignore))))

;; Tags with fast selection keys
(setq org-tag-alist (quote ((:startgroup)
			    (:endgroup)
			    ("PHONE" . ?p)
			    ("WAITING" . ?w)
			    ("HOLD" . ?h)
			    ("DIARY" . ?D)
			    ("PERSONAL" . ?m)
			    ("FINANCES" . ?F)
			    ("PROJECT" . ?P)
			    ("WORK" . ?W)
			    ("ORG" . ?O)
			    ("NORANG" . ?N)
			    ("crypt" . ?E)
			    ("MARK" . ?M)
			    ("NOTE" . ?n)
			    ("BZFLAG" . ?B)
			    ("CANCELLED" . ?c)
			    ("REFILE" . ?r)
			    ("FLAGGED" . ??))))

;; Sorting order for tasks on the agenda
(setq org-agenda-sorting-strategy
      (quote ((agenda habit-down time-up user-defined-up priority-down effort-up category-keep)
	      (todo priority-down effort-up category-up)
	      (tags priority-down effort-up category-up)
	      (search category-up))))

;; Custom agenda command definitions
(setq org-agenda-custom-commands
      (quote (("N" "Notes" tags "NOTE"
	       ((org-agenda-overriding-header "Notes")
		(org-tags-match-list-sublevels t)))
	      ("h" "Habits" tags-todo "STYLE=\"habit\""
	       ((org-agenda-overriding-header "Habits")
		(org-agenda-sorting-strategy
		 '(todo-state-down effort-up category-keep))))
	      (" " "Agenda"
	       ((agenda "" nil)
		(tags "-REFILE/"
		      ((org-agenda-overriding-header "Tasks to Archive")
		       (org-agenda-skip-function 'bh/skip-non-archivable-tasks)
		       (org-tags-match-list-sublevels nil)))
		(tags "REFILE"
		      ((org-agenda-overriding-header "Tasks to Refile")
		       (org-tags-match-list-sublevels nil)))
		(tags-todo "-WAITING-CANCELLED/!STARTED"
			   ((org-agenda-overriding-header "Started")
			    (org-agenda-skip-function 'bh/skip-projects-and-habits)
			    (org-agenda-todo-ignore-scheduled t)
			    (org-agenda-todo-ignore-deadlines t)
			    (org-agenda-todo-ignore-with-date t)
			    (org-tags-match-list-sublevels t)
			    (org-agenda-sorting-strategy 
			     '(priority-down effort-up category-keep))))
		(tags-todo "-CANCELLED/!WAITING|HOLD|DELEGATED|DEFERRED"
			   ((org-agenda-overriding-header "Waiting and Postponed Tasks")
			    (org-tags-match-list-sublevels nil)
			    (org-agenda-todo-ignore-scheduled 'future)
			    (org-agenda-todo-ignore-deadlines 'future)))
		(tags-todo "-WAITING-CANCELLED-DEFERRED-DELEGATED/!NEXT"
			   ((org-agenda-overriding-header "Next")
			    (org-agenda-skip-function 'bh/skip-projects-and-habits)
			    (org-agenda-todo-ignore-scheduled t)
			    (org-agenda-todo-ignore-deadlines t)
			    (org-agenda-todo-ignore-with-date t)
			    (org-tags-match-list-sublevels t)
			    (org-agenda-sorting-strategy
			     '(priority-down effort-up category-keep))))
		(tags-todo "-REFILE-CANCELLED/!TODO"
			   ((org-agenda-overriding-header "Backlog")
			    (org-agenda-skip-function 'bh/skip-project-tasks-maybe)
			    (org-agenda-todo-ignore-scheduled t)
			    (org-agenda-todo-ignore-deadlines t)
			    (org-agenda-todo-ignore-with-date t)
			    (org-agenda-sorting-strategy
			     '(priority-down effort-up category-keep))))
		(tags-todo "-HOLD-CANCELLED/!"
			   ((org-agenda-overriding-header "Projects")
			    (org-agenda-skip-function 'bh/skip-non-projects)
			    (org-agenda-sorting-strategy
			     '(category-keep))))
		(tags-todo "-REFILE-CANCELLED/!-HOLD-WAITING-DELEGATED-STARTED"
			   ((org-agenda-overriding-header "Ideas")
			    (org-agenda-skip-function 'bh/get-ideas)
			    (org-agenda-todo-ignore-scheduled t)
			    (org-agenda-todo-ignore-deadlines t)
			    (org-agenda-todo-ignore-with-date t)
			    (org-agenda-sorting-strategy
			     '(priority-down effort-up category-keep))))
		(tags-todo "-CANCELLED/!"
			   ((org-agenda-overriding-header "Stuck Projects")
			    (org-agenda-skip-function 'bh/skip-non-stuck-projects))))
	       nil)
	      ("A" "Tasks to Archive" tags "-REFILE/"
	       ((org-agenda-overriding-header "Tasks to Archive")
		(org-agenda-skip-function 'bh/skip-non-archivable-tasks)
		(org-tags-match-list-sublevels nil)))
	      ("r" "Tasks to Refile" tags "REFILE"
	       ((org-agenda-overriding-header "Tasks to Refile")
		(org-tags-match-list-sublevels nil)))
	      ("w" "Waiting Tasks" tags-todo "-CANCELLED/!WAITING|HOLD!DELEGATED|DEFERRED"
	       ((org-agenda-overriding-header "Waiting and Postponed tasks"))
	       (org-tags-match-list-sublevels nil))
	      ("#" "Stuck Projects" tags-todo "-CANCELLED/!"
	       ((org-agenda-overriding-header "Stuck Projects")
		(org-agenda-skip-function 'bh/skip-non-stuck-projects)))
	      ("s" "Started Tasks" tags-todo "-WAITING-CANCELLED/!STARTED"
	       ((org-agenda-overriding-header "Started Tasks")
		(org-agenda-skip-function 'bh/skip-projects-and-habits)
		(org-agenda-todo-ignore-scheduled t)
		(org-agenda-todo-ignore-deadlines t)
		(org-agenda-todo-ignore-with-date t)
		(org-tags-match-list-sublevels t)
		(org-agenda-sorting-strategy
		 '(todo-state-down effort-up category-keep))))
	      ("n" "Next Tasks" tags-todo "-WAITING-CANCELLED-DEFERRED-DELEGATED/!NEXT"
	       ((org-agenda-overriding-header "Next Tasks")
		(org-agenda-skip-function 'bh/skip-projects-and-habits)
		(org-agenda-todo-ignore-scheduled t)
		(org-agenda-todo-ignore-deadlines t)
		(org-agenda-todo-ignore-with-date t)
		(org-tags-match-list-sublevels t)
		(org-agenda-sorting-strategy
		 '(todo-state-down effort-up category-keep))))
	      ("R" "Tasks" tags-todo "-REFILE-CANCELLED/!-HOLD-WAITING-DELEGATED-STARTED"
	       ((org-agenda-overriding-header "Tasks")
		(org-agenda-skip-function 'bh/skip-project-tasks-maybe)
		(org-agenda-sorting-strategy
		 '(category-keep))))
	      ("p" "Projects" tags-todo "-HOLD-CANCELLED/!"
	       ((org-agenda-overriding-header "Projects")
		(org-agenda-skip-function 'bh/skip-non-projects)
		(org-agenda-sorting-strategy
		 '(category-keep))))
	      )))

;; Mark parent tasks as started
(defvar bh/mark-parent-tasks-started t)
(defvar bh/organization-task-id "eb155a82-92b2-4f25-a3c6-0304591af2f9")

;; Functions
(defun bh/org-auto-exclude-function (tag)
  "Automatic task exclusion in the agenda with / RET"
  (and (cond
	((string= tag "hold")
	 t)
	((string= tag "waiting")
	 t))
       (concat "-" tag)))

;; Exclude DONE state tasks from refile targets
(defun bh/verify-refile-target ()
  "Exclude todo keywords with a done state from refile targets"
  (not (member (nth 2 (org-heading-components)) org-done-keywords)))

;; Remove empty LOGBOOK drawers on clock out
(defun bh/remove-empty-drawer-on-clock-out ()
  (interactive)
  (save-excursion
    (beginning-of-line 0)
    (org-remove-empty-drawer-at "LOGBOOK" (point))))

(defun bh/mail-subtree ()
  (interactive)
  (org-mark-subtree)
  (org-mime-subtree))

(defun bh/hide-other ()
  (interactive)
  (save-excursion
    (org-back-to-heading 'invisible-ok)
    (hide-other)
    (org-cycle)
    (org-cycle)
    (org-cycle)))

(defun bh/set-truncate-lines ()
  "Toggle value of truncate-lines and refresh window display."
  (interactive)
  (setq truncate-lines (not truncate-lines))
  ;; now refresh window display (an idiom from simple.el):
  (save-excursion
    (set-window-start (selected-window)
		      (window-start (selected-window)))))

(defun bh/make-org-scratch ()
  (interactive)
  (find-file "/tmp/publish/scratch.org")
  (gnus-make-directory "/tmp/publish"))

(defun bh/switch-to-scratch ()
  (interactive)
  (switch-to-buffer "*scratch*"))

(defun bh/mark-parent-tasks-started ()
  "Visit each parent task and change TODO or NEXT states to STARTED"
  (unless bh/mark-parent-tasks-started
    (when (equal org-state "STARTED")
      (let ((bh/mark-parent-tasks-started t))
	(save-excursion
	  (while (org-up-heading-safe)
	    (when (member (nth 2 (org-heading-components)) (list "TODO" "NEXT"))
	      (org-todo "STARTED"))))))))

(defun alt-clean-equal-signs ()
  "This function makes lines of = signs invisible."
  (goto-char (point-min))
  (let ((state buffer-read-only))
    (when state (setq buffer-read-only nil))
    (while (not (eobp))
      (search-forward-regexp "^=+$" nil 'move)
      (add-text-properties (match-beginning 0)
			   (match-end 0)
			   '(invisible t)))
    (when state (setq buffer-read-only t))))

(defun bh/clock-in-to-started (kw)
  "Switch a task from TODO or NEXT to STARTED when clocking in.
Skips capture tasks, projects, and subprojects.
Switch projects and subprojects from NEXT back to TODO"
  (when (not (and (boundp 'org-capture-mode) org-capture-mode))
    (cond
     ((and (member (org-get-todo-state) (list "TODO" "NEXT"))
	   (bh/is-task-p))
      "STARTED")
     ((and (member (org-get-todo-state) (list "STARTED"))
	   (bh/is-project-p))
      "TODO"))))

(defun bh/find-project-task ()
  "Move point to the parent (project) task if any"
  (save-restriction
    (widen)
    (let ((parent-task (save-excursion (org-back-to-heading 'invisible-ok) (point))))
      (while (org-up-heading-safe)
	(when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
	  (setq parent-task (point))))
      (goto-char parent-task)
      parent-task)))

(defun bh/punch-in (arg)
  "Start continuous clocking and set the default task to the
selected task.	If no task is selected set the Organization task
as the default task."
  (interactive "p")
  (setq bh/keep-clock-running t)
  (if (equal major-mode 'org-agenda-mode)
      ;; We're in the agenda
      (let* ((marker (org-get-at-bol 'org-hd-marker))
	     (tags (org-with-point-at marker (org-get-tags-at))))
	(if (and (eq arg 4) tags)
	    (org-agenda-clock-in '(16))
	  (bh/clock-in-organization-task-as-default)))
    ;; We are not in the agenda
    (save-restriction
      (widen)
      ;; Find the tags on the current task
      (if (and (equal major-mode 'org-mode) (not (org-before-first-heading-p)) (eq arg 1))
	  (org-clock-in '(16))
	(bh/clock-in-organization-task-as-default)))))

(defun bh/punch-out ()
  (interactive)
  (setq bh/keep-clock-running nil)
  (when (org-clock-is-active)
    (org-clock-out))
  (org-agenda-remove-restriction-lock))

(defun bh/clock-in-default-task ()
  (save-excursion
    (org-with-point-at org-clock-default-task
      (org-clock-in))))

(defun bh/clock-in-parent-task ()
  "Move point to the parent (project) task if any and clock in"
  (let ((parent-task))
    (save-excursion
      (save-restriction
	(widen)
	(while (and (not parent-task) (org-up-heading-safe))
	  (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
	    (setq parent-task (point))))
	(if parent-task
	    (org-with-point-at parent-task
	      (org-clock-in))
	  (when bh/keep-clock-running
	    (bh/clock-in-default-task)))))))

(defun bh/clock-in-organization-task-as-default ()
  (interactive)
  (org-with-point-at (org-id-find bh/organization-task-id 'marker)
    (org-clock-in '(16))))

(defun bh/clock-out-maybe ()
  (when (and bh/keep-clock-running
	     (not org-clock-clocking-in)
	     (marker-buffer org-clock-default-task)
	     (not org-clock-resolving-clocks-due-to-idleness))
    (bh/clock-in-parent-task)))

(defun bh/clock-in-task-by-id (id)
  "Clock in a task by id"
  (org-with-point-at (org-id-find id 'marker)
    (org-clock-in nil)))

(defun bh/clock-in-last-task (arg)
  "Clock in the interrupted task if there is one
Skip the default task and get the next one.
A prefix arg forces clock in of the default task."
  (interactive "p")
  (let ((clock-in-to-task
	 (cond
	  ((eq arg 4) org-clock-default-task)
	  ((and (org-clock-is-active)
		(equal org-clock-default-task (cadr org-clock-history)))
	   (caddr org-clock-history))
	  ((org-clock-is-active) (cadr org-clock-history))
	  ((equal org-clock-default-task (car org-clock-history)) (cadr org-clock-history))
	  (t (car org-clock-history)))))
    (org-with-point-at clock-in-to-task
      (org-clock-in nil))))

(defun bh/set-agenda-restriction-lock (arg)
  "Set restriction lock to current task subtree or file if prefix is specified"
  (interactive "p")
  (let* ((pom (or (org-get-at-bol 'org-hd-marker)
		  org-agenda-restrict-begin))
	 (tags (org-with-point-at pom (org-get-tags-at))))
    (let ((restriction-type (if (equal arg 4) 'file 'subtree)))
      (save-restriction
	(cond
	 ((equal major-mode 'org-agenda-mode)
	  (org-with-point-at pom
	    (org-agenda-set-restriction-lock restriction-type)))
	 ((and (equal major-mode 'org-mode) (org-before-first-heading-p))
	  (org-agenda-set-restriction-lock 'file))
	 (t
	  (org-with-point-at pom
	    (org-agenda-set-restriction-lock restriction-type))))))))

;; Phone capture template handling with BBDB lookup
;; Adapted from code by Gregory J. Grubbs
(defun bh/phone-call ()
  "Return name and company info for caller from bbdb lookup"
  (interactive)
  (let* (name rec caller)
    (setq name (completing-read "Who is calling? "
				(bbdb-hashtable)
				'bbdb-completion-predicate
				'confirm))
    (when (> (length name) 0)
      ;; Something was supplied - look it up in bbdb
      (setq rec
	    (or (first
		 (or (bbdb-search (bbdb-records) name nil nil)
		     (bbdb-search (bbdb-records) nil name nil)))
		name)))
    ;; Build the bbdb link if we have a bbdb record, otherwise just return the name
    (setq caller (cond ((and rec (vectorp rec))
			(let ((name (bbdb-record-name rec))
			      (company (bbdb-record-company rec)))
			  (concat "[[bbdb:"
				  name "]["
				  name "]]"
				  (when company
				    (concat " - " company)))))
		       (rec)
		       (t "NameOfCaller")))
    (insert caller)))

(defun bh/is-project-p ()
  "Any task with a todo keyword subtask"
  (save-restriction
    (widen)
    (let ((has-subtask)
	  (subtree-end (save-excursion (org-end-of-subtree t)))
	  (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
      (save-excursion
	(forward-line 1)
	(while (and (not has-subtask)
		    (< (point) subtree-end)
		    (re-search-forward "^\*+ " subtree-end t))
	  (when (member (org-get-todo-state) org-todo-keywords-1)
	    (setq has-subtask t))))
      (and is-a-task has-subtask))))

(defun bh/is-project-subtree-p ()
  "Any task with a todo keyword that is in a project subtree.
Callers of this function already widen the buffer view."
  (let ((task (save-excursion (org-back-to-heading 'invisible-ok)
			      (point))))
    (save-excursion
      (bh/find-project-task)
      (if (equal (point) task)
	  nil
	t))))

(defun bh/is-task-p ()
  "Any task with a todo keyword and no subtask"
  (save-restriction
    (widen)
    (let ((has-subtask)
	  (subtree-end (save-excursion (org-end-of-subtree t)))
	  (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
      (save-excursion
	(forward-line 1)
	(while (and (not has-subtask)
		    (< (point) subtree-end)
		    (re-search-forward "^\*+ " subtree-end t))
	  (when (member (org-get-todo-state) org-todo-keywords-1)
	    (setq has-subtask t))))
      (and is-a-task (not has-subtask)))))

(defun bh/is-subproject-p ()
  "Any task which is a subtask of another project"
  (let ((is-subproject)
	(is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
    (save-excursion
      (while (and (not is-subproject) (org-up-heading-safe))
	(when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
	  (setq is-subproject t))))
    (and is-a-task is-subproject)))

(defun bh/list-sublevels-for-projects-indented ()
  "Set org-tags-match-list-sublevels so when restricted to a subtree we list all subtasks.
  This is normally used by skipping functions where this variable is already local to the agenda."
  (if (marker-buffer org-agenda-restrict-begin)
      (setq org-tags-match-list-sublevels 'indented)
    (setq org-tags-match-list-sublevels nil))
  nil)

(defun bh/list-sublevels-for-projects ()
  "Set org-tags-match-list-sublevels so when restricted to a subtree we list all subtasks.
  This is normally used by skipping functions where this variable is already local to the agenda."
  (if (marker-buffer org-agenda-restrict-begin)
      (setq org-tags-match-list-sublevels t)
    (setq org-tags-match-list-sublevels nil))
  nil)

(defun bh/skip-non-stuck-projects ()
  "Skip trees that are not stuck projects"
  (bh/list-sublevels-for-projects-indented)
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (if (bh/is-project-p)
	  (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
		 (has-next ))
	    (save-excursion
	      (forward-line 1)
	      (while (and (not has-next) (< (point) subtree-end) (or (re-search-forward "^\\*+ NEXT " subtree-end t) (re-search-forward "^\\*+ STARTED " subtree-end t)))
		(unless (member "WAITING" (org-get-tags-at))
		  (setq has-next t))))
	    (if has-next
		next-headline
	      nil)) ;; a stuck project, has subtasks but no next task
	next-headline))))

(defun bh/skip-non-projects ()
  "Skip trees that are not projects"
  (bh/list-sublevels-for-projects-indented)
  (if (save-restriction
	(widen)
	(let ((subtree-end (save-excursion (org-end-of-subtree t))))
	  (if (bh/is-project-p)
	      nil
	    subtree-end)))
      (org-end-of-subtree t)))

(defun bh/skip-project-trees-and-habits ()
  "Skip trees that are projects"
  (save-restriction
    (widen)
    (let ((subtree-end (save-excursion (org-end-of-subtree t))))
      (cond
       ((bh/is-project-p)
	subtree-end)
       ((org-is-habit-p)
	subtree-end)
       (t
	nil)))))

(defun bh/skip-projects-and-habits ()
  "Skip trees that are projects, tasks that are habits, single non-project tasks"
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (cond
       ((org-is-habit-p)
	next-headline)
       ((bh/is-project-p)
	next-headline)
       (t
	nil)))))

(defun bh/skip-project-tasks-maybe ()
  "Show tasks related to the current restriction.
When restricted to a project, skip habits, NEXT tasks, ideas and loose tasks.
When not restricted, skip habits, NEXT tasks and ideas."
  (save-restriction
    (widen)
    (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
	   (next-headline (save-excursion (or (outline-next-heading) (point-max))))
	   (limit-to-project (marker-buffer org-agenda-restrict-begin)))
      (cond
       ((bh/is-project-p)
	next-headline)
       ((and (bh/is-task-p)
	     (member (org-get-todo-state) (list "NEXT")))
	next-headline)
       ((and (bh/is-task-p)
	     (member (org-get-todo-state) (list "PROJECT")))
	next-headline)
       ((and (bh/is-task-p)
	     (member (org-get-category) (list "Ideas")))
	next-headline)
       ((org-is-habit-p)
	subtree-end)
       ((and limit-to-project
	     (bh/is-project-subtree-p)
	     (member (org-get-todo-state) (list "NEXT")))
	subtree-end)
       (t
	nil)))))

(defun bh/get-ideas ()
  "Show ideas."
  (save-restriction
    (widen)
    (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
	   (next-headline (save-excursion (or (outline-next-heading) (point-max))))
	   (limit-to-project (marker-buffer org-agenda-restrict-begin)))
      (cond
       ((bh/is-project-p)
	next-headline)
       ((and (bh/is-task-p)
	     (member (org-get-todo-state) (list "NEXT")))
	next-headline)
       ((and (bh/is-task-p)
	     (member (org-get-todo-state) (list "PROJECT")))
	next-headline)
       ((and (bh/is-task-p)
	     (not (member (org-get-category) (list "Ideas"))))
	next-headline)
       ((org-is-habit-p)
	subtree-end)
       ((and limit-to-project
	     (bh/is-project-subtree-p)
	     (member (org-get-todo-state) (list "NEXT")))
	subtree-end)
       (t
	nil)))))

(defun bh/skip-non-subprojects ()
  "Skip trees that are not projects"
  (let ((next-headline (save-excursion (outline-next-heading))))
    (if (bh/is-subproject-p)
	nil
      next-headline)))

(defun bh/skip-non-archivable-tasks ()
  "Skip trees that are not available for archiving"
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      ;; Consider only tasks with done todo headings as archivable candidates
      (if (member (org-get-todo-state) org-done-keywords)
	  (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
		 (daynr (string-to-int (format-time-string "%d" (current-time))))
		 (a-month-ago (* 60 60 24 (+ daynr 1)))
		 (last-month (format-time-string "%Y-%m-" (time-subtract (current-time) (seconds-to-time a-month-ago))))
		 (this-month (format-time-string "%Y-%m-" (current-time)))
		 (subtree-is-current (save-excursion
				       (forward-line 1)
				       (and (< (point) subtree-end)
					    (re-search-forward (concat last-month "\\|" this-month) subtree-end t)))))
	    (if subtree-is-current
		next-headline ;; Has a date in this month or last month, skip it
	      nil))	 ;; available to archive
	(or next-headline (point-max))))))

;; Erase all reminders and rebuilt reminders for today from the agenda
(defun bh/org-agenda-to-appt ()
  (interactive)
  (setq appt-time-msg-list nil)
  (org-agenda-to-appt))

(defun bh/org-todo (arg)
  (interactive "p")
  (if (equal arg 4)
      (save-restriction
	(widen)
	(org-narrow-to-subtree)
	(org-show-todo-tree nil))
    (widen)
    (org-narrow-to-subtree)
    (org-show-todo-tree nil)))

(defun bh/widen ()
  (interactive)
  (if (equal major-mode 'org-agenda-mode)
      (org-agenda-remove-restriction-lock)
    (widen)
    (org-agenda-remove-restriction-lock)))

(defun bh/restrict-to-file-or-follow (arg)
  "Set agenda restriction to 'file or with argument invoke follow mode.
I don't use follow mode very often but I restrict to file all the time
so change the default 'F' binding in the agenda to allow both"
  (interactive "p")
  (if (equal arg 4)
      (org-agenda-follow-mode)
    (if (equal major-mode 'org-agenda-mode)
	(bh/set-agenda-restriction-lock 4)
      (widen))))

(defun bh/narrow-to-org-subtree ()
  (widen)
  (org-narrow-to-subtree)
  (save-restriction
    (org-agenda-set-restriction-lock)))

(defun bh/narrow-to-subtree ()
  (interactive)
  (if (equal major-mode 'org-agenda-mode)
      (org-with-point-at (org-get-at-bol 'org-hd-marker)
	(bh/narrow-to-org-subtree))
    (bh/narrow-to-org-subtree)))

(defun bh/narrow-up-one-org-level ()
  (widen)
  (save-excursion
    (outline-up-heading 1 'invisible-ok)
    (bh/narrow-to-org-subtree)))

(defun bh/narrow-up-one-level ()
  (interactive)
  (if (equal major-mode 'org-agenda-mode)
      (org-with-point-at (org-get-at-bol 'org-hd-marker)
	(bh/narrow-up-one-org-level))
    (bh/narrow-up-one-org-level)))

(defun bh/narrow-to-org-project ()
  (widen)
  (save-excursion
    (bh/find-project-task)
    (bh/narrow-to-org-subtree)))

(defun bh/narrow-to-project ()
  (interactive)
  (if (equal major-mode 'org-agenda-mode)
      (org-with-point-at (org-get-at-bol 'org-hd-marker)
	(bh/narrow-to-org-project))
    (bh/narrow-to-org-project)))

(defun bh/agenda-sort (a b)
  "Sorting strategy for agenda items.
Late deadlines first, then scheduled, then non-late deadlines"
  (let (result num-a num-b)
    (cond
     ;; time specific items are already sorted first by org-agenda-sorting-strategy
     ;; non-deadline and non-scheduled items next
     ((bh/agenda-sort-test 'bh/is-not-scheduled-or-deadline a b))
     ;; deadlines for today next
     ((bh/agenda-sort-test 'bh/is-due-deadline a b))
     ;; late deadlines next
     ((bh/agenda-sort-test-num 'bh/is-late-deadline '< a b))
     ;; scheduled items for today next
     ((bh/agenda-sort-test 'bh/is-scheduled-today a b))
     ;; late scheduled items next
     ((bh/agenda-sort-test-num 'bh/is-scheduled-late '> a b))
     ;; pending deadlines last
     ((bh/agenda-sort-test-num 'bh/is-pending-deadline '< a b))
     ;; finally default to unsorted
     (t (setq result nil)))
    result))

(defmacro bh/agenda-sort-test (fn a b)
  "Test for agenda sort"
  `(cond
    ;; if both match leave them unsorted
    ((and (apply ,fn (list ,a))
	  (apply ,fn (list ,b)))
     (setq result nil))
    ;; if a matches put a first
    ((apply ,fn (list ,a))
     (setq result -1))
    ;; otherwise if b matches put b first
    ((apply ,fn (list ,b))
     (setq result 1))
    ;; if none match leave them unsorted
    (t nil)))

(defmacro bh/agenda-sort-test-num (fn compfn a b)
  `(cond
    ((apply ,fn (list ,a))
     (setq num-a (string-to-number (match-string 1 ,a)))
     (if (apply ,fn (list ,b))
	 (progn
	   (setq num-b (string-to-number (match-string 1 ,b)))
	   (setq result (if (apply ,compfn (list num-a num-b))
			    -1
			  1)))
       (setq result -1)))
    ((apply ,fn (list ,b))
     (setq result 1))
    (t nil)))

(defun bh/is-not-scheduled-or-deadline (date-str)
  (and (not (bh/is-deadline date-str))
       (not (bh/is-scheduled date-str))))

(defun bh/is-due-deadline (date-str)
  (string-match "Deadline:" date-str))

(defun bh/is-late-deadline (date-str)
  (string-match "In *\\(-.*\\)d\.:" date-str))

(defun bh/is-pending-deadline (date-str)
  (string-match "In \\([^-]*\\)d\.:" date-str))

(defun bh/is-deadline (date-str)
  (or (bh/is-due-deadline date-str)
      (bh/is-late-deadline date-str)
      (bh/is-pending-deadline date-str)))

(defun bh/is-scheduled (date-str)
  (or (bh/is-scheduled-today date-str)
      (bh/is-scheduled-late date-str)))

(defun bh/is-scheduled-today (date-str)
  (string-match "Scheduled:" date-str))

(defun bh/is-scheduled-late (date-str)
  (string-match "Sched\.\\(.*\\)x:" date-str))

(defun bh/show-org-agenda ()
  (interactive)
  (switch-to-buffer "*Org Agenda*")
  (delete-other-windows))

(defun bh/insert-inactive-timestamp ()
  (interactive)
  (org-insert-time-stamp nil t t nil nil nil))

(defun bh/insert-heading-inactive-timestamp ()
  (save-excursion
    (org-return)
    (org-cycle)
    (bh/insert-inactive-timestamp)))

(defun bh/prepare-meeting-notes ()
  "Prepare meeting notes for email
   Take selected region and convert tabs to spaces, mark TODOs with leading >>>, and copy to kill ring for pasting"
  (interactive)
  (let (prefix)
    (save-excursion
      (save-restriction
	(narrow-to-region (region-beginning) (region-end))
	(untabify (point-min) (point-max))
	(goto-char (point-min))
	(while (re-search-forward "^\\( *-\\\) \\(TODO\\|DONE\\): " (point-max) t)
	  (replace-match (concat (make-string (length (match-string 1)) ?>) " " (match-string 2) ": ")))
	(goto-char (point-min))
	(kill-ring-save (point-min) (point-max))))))

(defun bh/mark-next-parent-tasks-todo ()
  "Visit each parent task and change NEXT states to TODO"
  (let ((mystate (or (and (fboundp 'state)
			  state)
		     (nth 2 (org-heading-components)))))
    (when (equal mystate "NEXT")
      (save-excursion
	(while (org-up-heading-safe)
	  (when (member (nth 2 (org-heading-components)) (list "NEXT"))
	    (org-todo "TODO")))))))

;; hooks
;; orgstruct++-mode is enabled in Gnus message buffers to aid in creating
;; structured email messages.
(add-hook 'message-mode-hook 'orgstruct++-mode 'append)
(add-hook 'message-mode-hook 'turn-on-auto-fill 'append)
(add-hook 'message-mode-hook 'bbdb-define-all-aliases 'append)
(add-hook 'message-mode-hook 'orgtbl-mode 'append)
(add-hook 'message-mode-hook 'turn-on-flyspell 'append)
(add-hook 'message-mode-hook
	  '(lambda () (setq fill-column 72))
	  'append)
(add-hook 'message-mode-hook
	  '(lambda () (local-set-key (kbd "C-c M-o") 'org-mime-htmlize))
	  'append)

;; flyspell-mode is enabled for almost everything to help prevent creating
;; documents with spelling errors.
;; flyspell mode for spell checking everywhere
(add-hook 'org-mode-hook 'turn-on-flyspell 'append)

;; Disable C-c [ and C-c ] in org-mode
(add-hook 'org-mode-hook
	  (lambda ()
	    ;; Undefine C-c [ and C-c ] since this breaks my
	    ;; org-agenda files when directories are include It
	    ;; expands the files in the directories individually
	    (org-defkey org-mode-map "\C-c["	'undefined)
	    (org-defkey org-mode-map "\C-c]"	'undefined))
	  'append)

(add-hook 'org-mode-hook
	  (lambda ()
	    (local-set-key (kbd "C-c M-o") 'bh/mail-subtree))
	  'append)

;; Enable abbrev-mode
(add-hook 'org-mode-hook (lambda () (abbrev-mode 1)))

;; Remove empty LOGBOOK drawers on clock out
(add-hook 'org-clock-out-hook 'bh/remove-empty-drawer-on-clock-out 'append)

(add-hook 'org-after-todo-state-change-hook 'bh/mark-parent-tasks-started 'append)
(add-hook 'diary-display-hook 'fancy-diary-display)
(add-hook 'today-visible-calendar-hook 'calendar-mark-today)

(add-hook 'fancy-diary-display-mode-hook
	  '(lambda ()
	     (alt-clean-equal-signs)))

(add-hook 'remember-mode-hook 'org-remember-apply-template)
(add-hook 'org-clock-out-hook 'bh/clock-out-maybe 'append)

;; Narrowing
(add-hook 'org-agenda-mode-hook
	  '(lambda () (org-defkey org-agenda-mode-map "W" 'bh/widen))
	  'append)

(add-hook 'org-agenda-mode-hook
	  '(lambda () (org-defkey org-agenda-mode-map "F" 'bh/restrict-to-file-or-follow))
	  'append)

(add-hook 'org-agenda-mode-hook
	  '(lambda () (org-defkey org-agenda-mode-map "N" 'bh/narrow-to-subtree))
	  'append)

(add-hook 'org-agenda-mode-hook
	  '(lambda () (org-defkey org-agenda-mode-map "U" 'bh/narrow-up-one-level))
	  'append)

(add-hook 'org-agenda-mode-hook
	  '(lambda () (org-defkey org-agenda-mode-map "P" 'bh/narrow-to-project))
	  'append)

(add-hook 'org-agenda-mode-hook
	  '(lambda () (org-defkey org-agenda-mode-map "\C-c\C-x<" 'bh/set-agenda-restriction-lock))
	  'append)

;; Always hilight the current agenda line
(add-hook 'org-agenda-mode-hook
	  '(lambda () (hl-line-mode 1))
	  'append)

(add-hook 'org-agenda-mode-hook
	  (lambda ()
	    (define-key org-agenda-mode-map "q" 'bury-buffer))
	  'append)

(add-hook 'org-insert-heading-hook 'bh/insert-heading-inactive-timestamp 'append)
(add-hook 'org-after-todo-state-change-hook 'bh/mark-next-parent-tasks-todo 'append)
(add-hook 'org-clock-in-hook 'bh/mark-next-parent-tasks-todo 'append)

;; Rebuild the reminders everytime the agenda is displayed
(add-hook 'org-finalize-agenda-hook 'bh/org-agenda-to-appt 'append)

;; This is at the end of my .emacs - so appointments are set up when Emacs starts
(bh/org-agenda-to-appt)

;; Activate appointments so we get notifications
(appt-activate t)

;; If we leave Emacs running overnight - reset the appointments one minute after midnight
(run-at-time "24:01" nil 'bh/org-agenda-to-appt)
(run-at-time "07:00" 86400 '(lambda () (setq org-habit-show-habits t)))

;; Skeletons
;; sblk - Generic block #+begin_FOO .. #+end_FOO
(define-skeleton skel-org-block
  "Insert an org block, querying for type."
  "Type: "
  "#+begin_" str "\n"
  _ - \n
  "#+end_" str "\n")

(define-abbrev org-mode-abbrev-table "sblk" "" 'skel-org-block)

(setq org-mobile-agendas (quote all))
(setq org-mobile-directory "/scp:i02sopop@ritho:/var/www/org")
(setq org-mobile-encryption-password "")
(setq org-mobile-use-encryption nil)
