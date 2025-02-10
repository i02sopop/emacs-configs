;;;;;;;;;;;;;;
;; org-mode ;;
;;;;;;;;;;;;;;

(use-package org
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\|txt\\)$" . org-mode))

  (require 'org-crypt)

  ;; Encrypt all entries before saving
  (org-crypt-use-before-save-magic)
  (setq org-tags-exclude-from-inheritance (quote ("crypt")))

  ;; GPG key to use for encryption
  (setq org-crypt-disable-auto-save nil)

  ;; Mark parent tasks as started
  (defvar rt/mark-parent-tasks-started t)
  (defvar rt/organization-task-id "eb155a82-92b2-4f25-a3c6-0304591af2f9")

  ;; Load custom functions
  (load "~/.emacs.d/org-mode-functions.el")

  ;; Custom Key Bindings
  (global-set-key (kbd "\C-ca") 'org-agenda)
  (global-set-key (kbd "\C-x\C-a") 'org-archive-subtree)
  (global-set-key (kbd "\C-cc") 'org-capture)
  (global-set-key (kbd "\C-c\C-c") 'org-clock-in)
  (global-set-key (kbd "\C-cd") 'rt/org-mark-task-as-done)
  (global-set-key (kbd "\C-cg") 'org-clock-goto)
  (global-set-key (kbd "\C-ch") 'rt/highlight)
  (global-set-key (kbd "\C-ci") 'rt/punch-in)
  (global-set-key (kbd "\C-cl") 'org-store-link)
  (global-set-key (kbd "\C-cn") 'org-narrow-to-subtree)
  (global-set-key (kbd "\C-co") 'rt/punch-out)
  (global-set-key (kbd "\C-cp") 'org-pomodoro)
  (global-set-key (kbd "\C-c\C-p") 'rt/insert-inactive-timestamp)
  (global-set-key (kbd "\C-cr") 'org-capture)
  (global-set-key (kbd "\C-cs") 'rt/show-org-agenda)
  (global-set-key (kbd "\C-c\C-s") 'rt/switch-to-scratch)
  (global-set-key (kbd "\C-cT") 'rt/org-todo)
  (global-set-key (kbd "\C-cw") 'widen)
  (global-set-key (kbd "\C-c SPC") 'rt/clock-in-last-task)
  (global-set-key (kbd "<f12>") 'org-agenda)

  ;; Set variables
  (setq remember-annotation-functions (quote (org-remember-annotation)))
  (setq remember-handler-functions (quote (org-remember-handler)))
  (setq org-directory "~/org/agenda")
  (setq org-default-notes-file "~/org/agenda/notes.org")
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
  (setq org-hide-emphasis-markers t)

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
				(sequence "TODO(t)" "NEXT(n)" "STARTED(s)" "REVIEW(r)" "DONE(d)")
				(sequence "WAITING(w)" "DELEGATED(l)" "HOLD(h)" "DEFERRED(f)" "CANCELLED(c)"))))

  (setq org-todo-keyword-faces
		(quote (("TODO" :foreground "red" :weight bold)
				("NEXT" :foreground "blue" :weight bold)
				("STARTED" :foreground "yellow" :weight bold)
				("DONE" :foreground "green" :weight bold)
				("REVIEW" :foreground "yellow" :weight bold)
				("WAITING" :foreground "orange" :weight bold)
				("DEFERRED" :foreground "magenta" :weight bold)
				("HOLD" :foreground "orange" :weight bold)
				("DELEGATED" :foreground "green" :weight bold)
				("CANCELLED" :foreground "green" :weight bold)
				("PROJECT" :foreground "yellow" :weight bold)
				("FINISHED" :foreground "green" :weight bold))))

  (setq org-todo-state-tags-triggers
		(quote (("CANCELLED" ("CANCELLED" . t))
				("WAITING" ("WAITING" . t))
				("HOLD" ("WAITING" . t) ("HOLD" . t))
				("REVIEW" ("REVIEW") ("WAITING" . t))
				(done ("WAITING") ("HOLD") ("REVIEW"))
				("TODO" ("WAITING") ("CANCELLED") ("HOLD") ("REVIEW"))
				("STARTED" ("WAITING") ("CANCELLED") ("HOLD") ("REVIEW"))
				("NEXT" ("WAITING") ("CANCELLED") ("HOLD") ("REVIEW"))
				("DONE" ("WAITING") ("CANCELLED") ("HOLD") ("REVIEW")))))

  ;; Capture templates for: TODO tasks, Notes, appointments, phone calls, and org-protocol
  (setq org-capture-templates
		(quote (("t" "todo" entry (file "~/org/agenda/refile.org")
				 "* TODO %?\n:PROPERTIES:\n:ORDERED: t\n:LOGGING: TODO(!) NEXT(!) STARTED(!) REVIEW(!) WAITING(!) DELEGATED(!) HOLD(!) DONE(!) DEFERRED(!) CANCELLED(!) PHONE(!) PROJECT(!) FINISHED(!)\n:DESCRIPTION: -\n:ASSIGNED: ritho\n:DIFICULTY: -\n:CREATION_DATE: %U\n:NOTES: -\n:END:\n%a\n" :clock-in t :clock-resume t)
				("r" "respond" entry (file "~/org/agenda/refile.org")
				 "* TODO Respond to %:from on %:subject\n:PROPERTIES:\n:ORDERED: t\n:LOGGING: TODO(!) NEXT(!) REVIEW(!) STARTED(!) WAITING(!) DELEGATED(!) HOLD(!) DONE(!) DEFERRED(!) CANCELLED(!) PHONE(!) PROJECT(!) FINISHED(!)\n:DESCRIPTION: -\nASSIGNED: ritho\n:DIFICULTY: Easy\n:CREATION_DATE: %U\n:NOTES: -\n:END:\n%a\n" :clock-in t :clock-resume t :immediate-finish t)
				("n" "note" entry (file "~/org/agenda/notes.org")
				 "* %? :NOTE:\n:PROPERTIES:\n:CREATION_DATE: %U\n:STYLE: note\n:END:\n%a\n" :clock-in t :clock-resume t)
				("j" "Journal" entry (file+datetree "~/org/agenda/diary.org")
				 "* %?\n%U\n" :clock-in t :clock-resume t)
				("w" "org-protocol" entry (file "~/org/agenda/refile.org")
				 "* TODO Review %c\n%U\n" :immediate-finish t)
				("p" "Phone call" entry (file "~/org/agenda/refile.org")
				 "* PHONE %? :PHONE:\n%U" :clock-in t :clock-resume t)
				("h" "Habit" entry (file "~/org/agenda/habits.org")
				 "* NEXT %?\n%U\n%a\nSCHEDULED: %t .+1d\n:PROPERTIES:\n:ORDERED: t\n:LOGGING: TODO(!) NEXT(!) REVIEW(!) STARTED(!) WAITING(!) DELEGATED(!) HOLD(!) DONE(!) DEFERRED(!) CANCELLED(!) PHONE(!) PROJECT(!) FINISHED(!)\n:DESCRIPTION: -\n:ASSIGNED: ritho\n:CREATION_DATE: %U\n:NOTES: -\n:STYLE: habits\n:REPEAT_TO_STATE: NEXT\n:END:\n")
				("f" "Follow up" entry (file+olp "todo/todo.org" "Email" "Follow Up")
				 "* TODO Follow up with %(eval sent-message-to) on [[mu4e:msgid:%(eval sent-message-id)][%(eval sent-subject)]] SCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+3d\")) %i"))))

  ;; Calendar
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

  ;; Refile settings
  (setq org-refile-target-verify-function 'rt/verify-refile-target)

  ;; Do not dim blocked tasks
  (setq org-agenda-dim-blocked-tasks nil)

  ;; Compact the block agenda view
  (setq org-agenda-compact-blocks t)
  (setq org-agenda-auto-exclude-function 'rt/org-auto-exclude-function)

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
  (setq org-clock-in-switch-to-state 'rt/clock-in-to-started)

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
  (setq rt/keep-clock-running nil)
  (setq org-clock-sound "~/.emacs.d/notify.wav")

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
  (setq org-agenda-cmp-user-defined 'rt/agenda-sort)
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

  (setq org-link-frame-setup (quote ((file . find-file))))

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
  (setq org-modules (quote (;; org-bbdb
							org-crypt
							org-ctags
							org-habit
							org-id
							;; org-git-link
							org-habit
							org-inlinetask
							org-protocol
							;; org-mu4e
							mu4e-org)))

  (setq org-speed-commands (quote (("0" . ignore)
								   ("1" . ignore)
								   ("2" . ignore)
								   ("3" . ignore)
								   ("4" . ignore)
								   ("5" . ignore)
								   ("6" . ignore)
								   ("7" . ignore)
								   ("8" . ignore)
								   ("9" . ignore)
								   ("a" . org-archive-subtree)
								   ("b" . ignore)
								   ("c" . org-clock-in)
								   ("d" . rt/org-mark-task-as-done)
								   ("e" . ignore)
								   ("f" . ignore)
								   ("g" . org-clock-goto)
								   ("h" . rh/highlight)
								   ("i" . rt/punch-in)
								   ("k" . org-kill-note-or-show-branches)
								   ("l" . org-store-link)
								   ("m" . ignore)
								   ("n" . org-narrow-to-subtree)
								   ("o" . rt/punch-out)
								   ("p" . org-pomodoro)
								   ("q" . rt/show-org-agenda)
								   ("r" . org-capture)
								   ("s" . org-save-all-org-buffers)
								   ("t" . ignore)
								   ("w" . org-refile)
								   ("x" . ignore)
								   ("y" . ignore)
								   ("z" . org-add-note)
								   ("A" . ignore)
								   ("B" . ignore)
								   ("E" . ignore)
								   ("F" . rt/restrict-to-file-or-follow)
								   ("G" . ignore)
								   ("H" . ignore)
								   ("I" . rt/clock-in-last-task)
								   ("J" . org-clock-goto)
								   ("K" . ignore)
								   ("L" . ignore)
								   ("M" . ignore)
								   ("N" . ignore)
								   ("P" . ignore)
								   ("Q" . ignore)
								   ("R" . ignore)
								   ("S" . ignore)
								   ("T" . rt/org-todo)
								   ("U" . ignore)
								   ("V" . ignore)
								   ("W" . rt/widen)
								   ("X" . ignore)
								   ("Y" . ignore)
								   ("Z" . ignore))))

  ;; Tags with fast selection keys
  (setq org-tag-alist (quote ((:startgroup)
							  (:endgroup)
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
							  ("REFILE" . ?R)
							  ("read" . ?r)
							  ("view" . ?v)
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
				  (org-tags-match-list-sublevels nil)))
				("h" "Habits" tags-todo "STYLE=\"habit\""
				 ((org-agenda-overriding-header "Habits")
				  (org-agenda-sorting-strategy
				   '(todo-state-down effort-up category-keep))))
				("A" "Tasks to Archive" tags "-REFILE|read|view/"
				 ((org-agenda-overriding-header "Tasks to Archive")
				  (org-agenda-skip-function 'rt/skip-non-archivable-tasks)
				  (org-tags-match-list-sublevels nil)))
				("r" "Tasks to Refile" tags "REFILE"
				 ((org-agenda-overriding-header "Tasks to Refile")
				  (org-tags-match-list-sublevels nil)))
				("R" "Tasks to Read" tags "read"
				 ((org-agenda-overriding-header "Tasks to Read")
				  (org-tags-match-list-sublevels nil)))
				("W" "Tasks to Watch" tags "watch"
				 ((org-agenda-overriding-header "Tasks to Refile")
				  (org-tags-match-list-sublevels nil)))
				("w" "Waiting Tasks" tags-todo "-CANCELLED/!WAITING|HOLD!DELEGATED|DEFERRED"
				 ((org-agenda-overriding-header "Waiting and Postponed tasks"))
				 (org-tags-match-list-sublevels nil))
				("#" "Stuck Projects" tags-todo "-CANCELLED/!"
				 ((org-agenda-overriding-header "Stuck Projects")
				  (org-agenda-skip-function 'rt/skip-non-stuck-projects)))
				("s" "Started Tasks" tags-todo "-WAITING-CANCELLED/!STARTED"
				 ((org-agenda-overriding-header "Started Tasks")
				  (org-agenda-skip-function 'rt/skip-projects-and-habits)
				  (org-agenda-todo-ignore-scheduled t)
				  (org-agenda-todo-ignore-deadlines t)
				  (org-agenda-todo-ignore-with-date t)
				  (org-tags-match-list-sublevels t)
				  (org-agenda-sorting-strategy
				   '(todo-state-down effort-up category-keep))))
				("n" "Next Tasks" tags-todo "-WAITING-CANCELLED-DEFERRED-DELEGATED/!NEXT"
				 ((org-agenda-overriding-header "Next Tasks")
				  (org-agenda-skip-function 'rt/skip-projects-and-habits)
				  (org-agenda-todo-ignore-scheduled t)
				  (org-agenda-todo-ignore-deadlines t)
				  (org-agenda-todo-ignore-with-date t)
				  (org-tags-match-list-sublevels t)
				  (org-agenda-sorting-strategy
				   '(todo-state-down effort-up category-keep))))
				("T" "Tasks" tags-todo "-REFILE-CANCELLED/!-HOLD-WAITING-DELEGATED-STARTED"
				 ((org-agenda-overriding-header "Tasks")
				  (org-agenda-skip-function 'rt/skip-project-tasks-maybe)
				  (org-agenda-sorting-strategy
				   '(category-keep))))
				("p" "Projects" tags-todo "-HOLD-CANCELLED/!"
				 ((org-agenda-overriding-header "Projects")
				  (org-agenda-skip-function 'rt/skip-non-projects)
				  (org-agenda-sorting-strategy
				   '(category-keep))))
				)))

  ;; hooks
  (add-hook 'message-mode-hook 'turn-on-auto-fill 'append)
  (add-hook 'message-mode-hook 'orgtbl-mode 'append)
  (add-hook 'message-mode-hook 'turn-on-flyspell 'append)
  (add-hook 'message-mode-hook
			#'(lambda () (setq fill-column 72))
			'append)
  (add-hook 'message-mode-hook
			#'(lambda () (local-set-key (kbd "C-c M-o") 'org-mime-htmlize))
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
			  (local-set-key (kbd "C-c M-o") 'rt/mail-subtree))
			'append)

  ;; Enable abbrev-mode
  (add-hook 'org-mode-hook (lambda () (abbrev-mode 1)))

  ;; Remove empty LOGBOOK drawers on clock out
  (add-hook 'org-clock-out-hook 'rt/remove-empty-drawer-on-clock-out 'append)

  (add-hook 'org-after-todo-state-change-hook 'rt/mark-parent-tasks-started 'append)
  (add-hook 'diary-display-hook 'fancy-diary-display)
  (add-hook 'today-visible-calendar-hook 'calendar-mark-today)

  (add-hook 'fancy-diary-display-mode-hook
			#'(lambda ()
				(alt-clean-equal-signs)))

  (add-hook 'remember-mode-hook 'org-remember-apply-template)
  (add-hook 'org-clock-out-hook 'rt/clock-out-maybe 'append)

  (add-hook 'org-agenda-mode-hook
			#'(lambda () (org-defkey org-agenda-mode-map "F" 'rt/restrict-to-file-or-follow))
			'append)

  (add-hook 'org-agenda-mode-hook
			#'(lambda () (org-defkey org-agenda-mode-map "\C-c\C-x<" 'rt/set-agenda-restriction-lock))
			'append)

  ;; Always hilight the current agenda line
  (add-hook 'org-agenda-mode-hook
			#'(lambda () (hl-line-mode 1))
			'append)

  (add-hook 'org-agenda-mode-hook
			(lambda ()
			  (define-key org-agenda-mode-map "q" 'bury-buffer))
			'append)

  (add-hook 'org-insert-heading-hook 'rt/insert-heading-inactive-timestamp 'append)
  (add-hook 'org-after-todo-state-change-hook 'rt/mark-next-parent-tasks-todo 'append)
  (add-hook 'org-clock-in-hook 'rt/mark-next-parent-tasks-todo 'append)

  ;; Rebuild the reminders everytime the agenda is displayed
  (add-hook 'org-finalize-agenda-hook 'rt/org-agenda-to-appt 'append)

  ;; This is at the end of my .emacs - so appointments are set up when Emacs starts
  (rt/org-agenda-to-appt)

  ;; Activate appointments so we get notifications
  (appt-activate t)

  ;; If we leave Emacs running overnight - reset the appointments one minute after midnight
  (run-at-time "24:01" nil 'rt/org-agenda-to-appt)
  (run-at-time "07:00" 86400 #'(lambda () (setq org-habit-show-habits t)))

  ;; Skeletons
  ;; sblk - Generic block #+begin_FOO .. #+end_FOO
  (define-skeleton skel-org-block
	"Insert an org block, querying for type."
	"Type: "
	"#+begin_" str "\n"
	_ - \n
	"#+end_" str "\n")

  (define-abbrev org-mode-abbrev-table "sblk" "" 'skel-org-block)

  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

  (add-hook 'org-mode-hook 'visual-line-mode)

  (add-hook 'message-sent-hook #'my/org-capture-sent-mail)
  (defun my/org-capture-sent-mail ()
    "Prepare to capture sent mail after window configuration is reset."
    (let* ((sent-message-id
          (replace-regexp-in-string
           "[<>]" "" (message-fetch-field "Message-Id")))
           (sent-message-to
            (replace-regexp-in-string " <.*>" "" (message-fetch-field "To")))
         (sent-subject (or (message-fetch-field "Subject") "No subject")))
      (org-capture nil "efu")
      (add-hook 'mu4e-compose-post-hook #'my/pop-to-buffer-org-capture-mail 99)))

  (defun my/pop-to-buffer-org-capture-mail ()
    (pop-to-buffer
     (car (match-buffers
           (lambda (buffer)
             (equal "efu"
                    (plist-get
                     (buffer-local-value
                      'org-capture-current-plist buffer)
                     :key))))))
    (remove-hook 'mu4e-compose-post-hook #'my/pop-to-buffer-org-capture-mail))

  ;;;;;;;;;;;;;;;;;;;;;;;;
  ;; end of org config. ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;
  )

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

(use-package org-projectile
  :ensure t)

(use-package org-journal
  :ensure t
  :config
  (setq org-journal-dir "~/journal/")
  (setq org-journal-enable-encryption nil)

  (global-set-key (kbd "\C-cj") 'org-journal-new-entry))

(use-package org-mime
  :ensure t)

(use-package org-pomodoro
  :ensure t
  :config
  (setq org-pomodoro-keep-killed-pomodoro-time t)
  (setq org-pomodoro-length 50)
  (setq org-pomodoro-start-sound-p t))

(use-package pomodoro
  :ensure t)

(use-package org-present
  :ensure t)

(use-package org-table-comment
  :ensure t)

(use-package bbdb
  :ensure t)

(use-package bbdb-ext
  :ensure t)

(use-package bbdb2erc
  :ensure t)

(use-package calfw
  :ensure t)

(use-package calfw-cal
  :ensure t)

(use-package calfw-org
  :ensure t)

(use-package epresent
  :ensure t)

(use-package org-review
  :ensure t
  :config
  (setq org-agenda-custom-commands
		(append org-agenda-custom-commands
				(quote (("v" "Review projects" tags-todo "-CANCELLED/"
						((org-agenda-overriding-header "Reviews Scheduled")
						 (org-agenda-skip-function 'rt/skip-habits)
						 (org-agenda-cmp-user-defined 'rt/agenda-sort)
						 (org-agenda-sorting-strategy '(todo-state-down effort-up category-keep))))))))

  (add-hook 'org-agenda-mode-hook
            (lambda ()
              (local-set-key (kbd "C-c C-r")
                             'org-review-insert-last-review))))
(use-package org-wc
  :ensure t)

(use-package org-clock-today
  :ensure t
  :config
  (setq org-clock-today-hide-default-org-clock-mode-line 't)
  (org-clock-today-mode 1))

(use-package org-table-sticky-header
  :ensure t)

(use-package org-ac
  :ensure t
  :config
  (org-ac/config-default))

(use-package org-alert
  :ensure t
  :config
  (setq alert-default-style 'libnotify)
  (setq org-alert-interval 300
		org-alert-notify-cutoff 10
		org-alert-notify-after-event-cutoff 10))

(use-package org-make-toc
  :ensure t
  :config
  (add-hook 'org-mode-hook #'org-make-toc-mode))

(use-package org-notebook
  :ensure t)

(use-package org-notify
  :ensure t)

(use-package org-analyzer
  :ensure t)

(use-package org-books
  :ensure t
  :config
  (setq org-books-file "~/org/books.org")
  (global-set-key (kbd "\C-ci") 'org-books-add-isbn))

(use-package org-super-agenda
  :ensure t
  :config
  (org-super-agenda-mode 1)

  (setq org-super-agenda-groups
		'(
		  (:discard (:tag "scheduled"))
		  (:name "Tasks to refile" :tag "REFILE" :order 1)
		  (:name "Today" :scheduled today :order 2)
		  (:name "Due today" :deadline today :order 3)
		  (:name "Overdue" :deadline past :order 4)
		  (:name "Due soon" :deadline future :order 5)
		  (:name "Started" :todo "STARTED" :order 6)
		  (:name "Waiting" :todo "WAITING" :todo "HOLD" :order 7)
		  (:name "Delegated" :todo "DELEGATED" :order 8)
		  (:name "Posponed" :todo "DEFERRED" :order 9)

		  (:name "Review" :and (:todo "REVIEW" :not (:tag "read" :tag "view")) :order 10)
		  (:name "Next very important" :and (:todo "NEXT" :priority "A" :not (:tag "read" :tag "view")) :order 11)
		  (:name "Next important" :and (:todo "NEXT" :priority "B" :not (:tag "read" :tag "view")) :order 12)
		  (:name "Next unimportant" :and (:todo "NEXT" :priority "C" :not (:tag "read" :tag "view")) :order 13)
		  (:name "Next without priority" :and (:todo "NEXT" :not (:priority "A" :priority "B" :priority "C" :tag "read" :tag "view")) :order 14)

		  (:name "Backlog very important" :and (:todo "TODO" :priority "A" :not (:tag "read" :tag "view")) :order 15)
		  (:name "Backlog important" :and (:todo "TODO" :priority "B" :not (:tag "read" :tag "view")) :order 16)
		  (:name "Backlog unimportant" :and (:todo "TODO" :priority "C" :not (:tag "read" :tag "view")) :order 17)
		  (:name "Backlog without priority" :and (:todo "TODO" :not (:priority "A" :priority "B" :priority "C" :tag "read" :tag "view")) :order 18)

		  (:name "To read very important" :and (:tag "read" :priority "A") :order 19)
		  (:name "To read important" :and (:tag "read" :priority "B") :order 20)
		  (:name "To read unimportant" :and (:tag "read" :priority "C") :order 21)
		  (:name "To read without priority" :and (:tag "read" :not (:priority "A" :priority "B" :priority "C")) :order 22)

		  (:name "To view very important" :and (:tag "view" :priority "A") :order 23)
		  (:name "To view important" :and (:tag "view" :priority "B") :order 24)
		  (:name "To view unimportant" :and (:tag "view" :priority "C") :order 25)
		  (:name "To view without priority" :and (:tag "view" :not (:priority "A" :priority "B" :priority "C")) :order 26)

		  (:name "Cancelled" :todo "CANCELLED" :order 27)
		  (:name "Projects" :todo "PROJECT" :order 28)
		  )
		)

  (setq org-agenda-custom-commands
		(append org-agenda-custom-commands
				(quote (("g" "Supermode Agenda"
						 ((agenda "" ((org-super-agenda-groups
									   '((:name "Today" :time-grid t)))))
						  (tags "-REFILE|read|view/"
								((org-agenda-overriding-header "Tasks to Archive")
								 (org-agenda-skip-function 'rt/skip-non-archivable-tasks)
								 (org-tags-match-list-sublevels nil)))
						  (todo "" ((org-agenda-overriding-header "Tasks to work on")
									(org-super-agenda-groups org-super-agenda-groups))))
						 nil)))))
  )

(use-package org-roam
  :ensure t
  :config
  (setq org-roam-directory "~/org-roam/")
  (setq org-roam-dailies-directory "journal/")
  (setq org-roam-dailies-capture-templates
		'(("d" "default" entry "* %<%I:%M %p>: %?"
		   :if-new (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n"))))
  )

(use-package org-roam-ql
  :ensure t)

(use-package org-roam-ql-ql
  :ensure t)

(use-package org-roam-timestamps
  :ensure t)

(use-package org-roam-ui
  :ensure t)
