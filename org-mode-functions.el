;;;;;;;;;;;;;;;
;; Functions ;;
;;;;;;;;;;;;;;;
(defun rt/org-mark-task-as-done()
  (interactive)
  (org-todo "DONE")
  (org-archive-subtree))

(defun rt/org-todo (arg)
  (interactive "p")
  (if (equal arg 4)
	  (save-restriction
		(widen)
		(org-narrow-to-subtree)
		(org-show-todo-tree nil))
	(widen)
	(org-narrow-to-subtree)
	(org-show-todo-tree nil)))

(defun rt/widen ()
  (interactive)
  (if (equal major-mode 'org-agenda-mode)
	  (org-agenda-remove-restriction-lock)
	(widen)
	(org-agenda-remove-restriction-lock)))

(defun rt/show-org-agenda ()
  (interactive)
  (switch-to-buffer "*Org Agenda*")
  (delete-other-windows))

(defun rt/hilight ()
  (interactive)
  (save-excursion
	(org-back-to-heading 'invisible-ok)
	(hide-other)
	(org-cycle)
	(org-cycle)
	(org-cycle)))

(defun rt/switch-to-scratch ()
  (interactive)
  (switch-to-buffer "*scratch*"))

(defun rt/insert-inactive-timestamp ()
  (interactive)
  (org-insert-time-stamp nil t t nil nil nil))

(defun rt/insert-heading-inactive-timestamp ()
  (save-excursion
	(org-return)
	(org-cycle)
	(rt/insert-inactive-timestamp)))

(defun rt/mark-parent-tasks-started ()
  "Visit each parent task and change TODO or NEXT states to STARTED"
  (unless rt/mark-parent-tasks-started
	(when (equal org-state "STARTED")
	  (let ((rt/mark-parent-tasks-started t))
		(save-excursion
		  (while (org-up-heading-safe)
			(when (member (nth 2 (org-heading-components)) (list "TODO" "NEXT"))
			  (org-todo "STARTED"))))))))

(defun rt/punch-in (arg)
  "Start continuous clocking and set the default task to the
selected task.	If no task is selected set the Organization task
as the default task."
  (interactive "p")
  (setq rt/keep-clock-running t)
  (if (equal major-mode 'org-agenda-mode)
	  ;; We're in the agenda
	  (let* ((marker (org-get-at-bol 'org-hd-marker))
			 (tags (org-with-point-at marker (org-get-tags-at))))
		(if (and (eq arg 4) tags)
			(org-agenda-clock-in '(16))
		  (rt/clock-in-organization-task-as-default)))
	;; We are not in the agenda
	(save-restriction
	  (widen)
	  ;; Find the tags on the current task
	  (if (and (equal major-mode 'org-mode) (not (org-before-first-heading-p)) (eq arg 1))
		  (org-clock-in '(16))
		(rt/clock-in-organization-task-as-default)))))

(defun rt/punch-out ()
  (interactive)
  (setq rt/keep-clock-running nil)
  (when (org-clock-is-active)
	(org-clock-out))
  (org-agenda-remove-restriction-lock))

(defun rt/clock-in-default-task ()
  (save-excursion
	(org-with-point-at org-clock-default-task
	  (org-clock-in))))

(defun rt/clock-in-parent-task ()
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
		  (when rt/keep-clock-running
			(rt/clock-in-default-task)))))))

(defun rt/clock-in-organization-task-as-default ()
  (interactive)
  (org-with-point-at (org-id-find rt/organization-task-id 'marker)
	(org-clock-in '(16))))

(defun rt/clock-out-maybe ()
  (when (and rt/keep-clock-running
			 (not org-clock-clocking-in)
			 (marker-buffer org-clock-default-task)
			 (not org-clock-resolving-clocks-due-to-idleness))
	(rt/clock-in-parent-task)))

(defun rt/clock-in-task-by-id (id)
  "Clock in a task by id"
  (org-with-point-at (org-id-find id 'marker)
	(org-clock-in nil)))

(defun rt/clock-in-last-task (arg)
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

(defun rt/clock-in-to-started (kw)
  "Switch a task from TODO or NEXT to STARTED when clocking in.
Skips capture tasks, projects, and subprojects.
Switch projects and subprojects from NEXT back to TODO"
  (when (not (and (boundp 'org-capture-mode) org-capture-mode))
	(cond
	 ((and (member (org-get-todo-state) (list "TODO" "NEXT"))
		   (rt/is-task-p))
	  "STARTED")
	 ((and (member (org-get-todo-state) (list "STARTED"))
		   (rt/is-project-p))
	  "TODO"))))

;; Remove empty LOGBOOK drawers on clock out
(defun rt/remove-empty-drawer-on-clock-out ()
  (interactive)
  (save-excursion
	(beginning-of-line 0)
	(org-remove-empty-drawer-at "LOGBOOK" (point))))

(defun rt/find-project-task ()
  "Move point to the parent (project) task if any"
  (save-restriction
	(widen)
	(let ((parent-task (save-excursion (org-back-to-heading 'invisible-ok) (point))))
	  (while (org-up-heading-safe)
		(when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
		  (setq parent-task (point))))
	  (goto-char parent-task)
	  parent-task)))

(defun rt/org-auto-exclude-function (tag)
  "Automatic task exclusion in the agenda with / RET"
  (and (cond
		((string= tag "hold")
		 t)
		((string= tag "waiting")
		 t))
	   (concat "-" tag)))

;; Exclude DONE state tasks from refile targets
(defun rt/verify-refile-target ()
  "Exclude todo keywords with a done state from refile targets"
  (not (member (nth 2 (org-heading-components)) org-done-keywords)))

(defun rt/mail-subtree ()
  (interactive)
  (org-mark-subtree)
  (org-mime-subtree))

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

(defun rt/set-agenda-restriction-lock (arg)
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

(defun rt/is-project-p ()
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

(defun rt/is-project-subtree-p ()
  "Any task with a todo keyword that is in a project subtree.
Callers of this function already widen the buffer view."
  (let ((task (save-excursion (org-back-to-heading 'invisible-ok)
							  (point))))
	(save-excursion
	  (rt/find-project-task)
	  (if (equal (point) task)
		  nil
		t))))

(defun rt/is-task-p ()
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

(defun rt/is-subproject-p ()
  "Any task which is a subtask of another project"
  (let ((is-subproject)
		(is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
	(save-excursion
	  (while (and (not is-subproject) (org-up-heading-safe))
		(when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
		  (setq is-subproject t))))
	(and is-a-task is-subproject)))

(defun rt/list-sublevels-for-projects-indented ()
  "Set org-tags-match-list-sublevels so when restricted to a subtree we list all subtasks.
  This is normally used by skipping functions where this variable is already local to the agenda."
  (if (marker-buffer org-agenda-restrict-begin)
	  (setq org-tags-match-list-sublevels 'indented)
	(setq org-tags-match-list-sublevels nil))
  nil)

(defun rt/list-sublevels-for-projects ()
  "Set org-tags-match-list-sublevels so when restricted to a subtree we list all subtasks.
  This is normally used by skipping functions where this variable is already local to the agenda."
  (if (marker-buffer org-agenda-restrict-begin)
	  (setq org-tags-match-list-sublevels t)
	(setq org-tags-match-list-sublevels nil))
  nil)

(defun rt/skip-non-stuck-projects ()
  "Skip trees that are not stuck projects"
  (rt/list-sublevels-for-projects-indented)
  (save-restriction
	(widen)
	(let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
	  (if (rt/is-project-p)
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

(defun rt/skip-non-projects ()
  "Skip trees that are not projects"
  (rt/list-sublevels-for-projects-indented)
  (if (save-restriction
		(widen)
		(let ((subtree-end (save-excursion (org-end-of-subtree t))))
		  (if (rt/is-project-p)
			  nil
			subtree-end)))
	  (org-end-of-subtree t)))

(defun rt/skip-project-trees-and-habits ()
  "Skip trees that are projects"
  (save-restriction
	(widen)
	(let ((subtree-end (save-excursion (org-end-of-subtree t))))
	  (cond
	   ((rt/is-project-p)
		subtree-end)
	   ((org-is-habit-p)
		subtree-end)
	   (t
		nil)))))

(defun rt/skip-projects-and-habits ()
  "Skip trees that are projects, tasks that are habits, single non-project tasks"
  (save-restriction
	(widen)
	(let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
	  (cond
	   ((org-is-habit-p)
		next-headline)
	   ((rt/is-project-p)
		next-headline)
	   (t
		nil)))))

(defun rt/skip-habits ()
  "Skip trees that are projects, tasks that are habits, single non-project tasks"
  (save-restriction
	(widen)
	(let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
	  (cond
	   ((org-is-habit-p)
		next-headline)
	   (t
		nil)))))

(defun rt/skip-project-tasks-maybe ()
  "Show tasks related to the current restriction.
When restricted to a project, skip habits, NEXT tasks, ideas and loose tasks.
When not restricted, skip habits, NEXT tasks and ideas."
  (save-restriction
	(widen)
	(let* ((subtree-end (save-excursion (org-end-of-subtree t)))
		   (next-headline (save-excursion (or (outline-next-heading) (point-max))))
		   (limit-to-project (marker-buffer org-agenda-restrict-begin)))
	  (cond
	   ((rt/is-project-p)
		next-headline)
	   ((and (rt/is-task-p)
			 (member (org-get-todo-state) (list "NEXT")))
		next-headline)
	   ((and (rt/is-task-p)
			 (member (org-get-todo-state) (list "PROJECT")))
		next-headline)
	   ((and (rt/is-task-p)
			 (member (org-get-category) (list "Ideas")))
		next-headline)
	   ((org-is-habit-p)
		subtree-end)
	   ((and limit-to-project
			 (rt/is-project-subtree-p)
			 (member (org-get-todo-state) (list "NEXT")))
		subtree-end)
	   (t
		nil)))))

(defun rt/get-ideas ()
  "Show ideas."
  (save-restriction
	(widen)
	(let* ((subtree-end (save-excursion (org-end-of-subtree t)))
		   (next-headline (save-excursion (or (outline-next-heading) (point-max))))
		   (limit-to-project (marker-buffer org-agenda-restrict-begin)))
	  (cond
	   ((rt/is-project-p)
		next-headline)
	   ((and (rt/is-task-p)
			 (member (org-get-todo-state) (list "NEXT")))
		next-headline)
	   ((and (rt/is-task-p)
			 (member (org-get-todo-state) (list "PROJECT")))
		next-headline)
	   ((and (rt/is-task-p)
			 (not (member (org-get-category) (list "Ideas"))))
		next-headline)
	   ((org-is-habit-p)
		subtree-end)
	   ((and limit-to-project
			 (rt/is-project-subtree-p)
			 (member (org-get-todo-state) (list "NEXT")))
		subtree-end)
	   (t
		nil)))))

(defun rt/skip-non-subprojects ()
  "Skip trees that are not projects"
  (let ((next-headline (save-excursion (outline-next-heading))))
	(if (rt/is-subproject-p)
		nil
	  next-headline)))

(defun rt/skip-non-archivable-tasks ()
  "Skip trees that are not available for archiving"
  (save-restriction
	(widen)
	(let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
	  ;; Consider only tasks with done todo headings as archivable candidates
	  (if (member (org-get-todo-state) org-done-keywords)
		  (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
				 (daynr (string-to-number (format-time-string "%d" (current-time))))
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
(defun rt/org-agenda-to-appt ()
  (interactive)
  (setq appt-time-msg-list nil)
  (org-agenda-to-appt))

(defun rt/restrict-to-file-or-follow (arg)
  "Set agenda restriction to 'file or with argument invoke follow mode.
I don't use follow mode very often but I restrict to file all the time
so change the default 'F' binding in the agenda to allow both"
  (interactive "p")
  (if (equal arg 4)
	  (org-agenda-follow-mode)
	(if (equal major-mode 'org-agenda-mode)
		(rt/set-agenda-restriction-lock 4)
	  (widen))))

(defun rt/agenda-sort (a b)
  "Sorting strategy for agenda items.
Late deadlines first, then scheduled, then non-late deadlines"
  (let (result num-a num-b)
	(cond
	 ;; time specific items are already sorted first by org-agenda-sorting-strategy
	 ;; non-deadline and non-scheduled items next
	 ((rt/agenda-sort-test 'rt/is-not-scheduled-or-deadline a b))
	 ;; deadlines for today next
	 ((rt/agenda-sort-test 'rt/is-due-deadline a b))
	 ;; late deadlines next
	 ((rt/agenda-sort-test-num 'rt/is-late-deadline '< a b))
	 ;; scheduled items for today next
	 ((rt/agenda-sort-test 'rt/is-scheduled-today a b))
	 ;; late scheduled items next
	 ((rt/agenda-sort-test-num 'rt/is-scheduled-late '> a b))
	 ;; pending deadlines last
	 ((rt/agenda-sort-test-num 'rt/is-pending-deadline '< a b))
	 ;; finally default to unsorted
	 (t (setq result nil)))
	result))

(defmacro rt/agenda-sort-test (fn a b)
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

(defmacro rt/agenda-sort-test-num (fn compfn a b)
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

(defun rt/is-not-scheduled-or-deadline (date-str)
  (and (not (rt/is-deadline date-str))
	   (not (rt/is-scheduled date-str))))

(defun rt/is-due-deadline (date-str)
  (string-match "Deadline:" date-str))

(defun rt/is-late-deadline (date-str)
  (string-match "In *\\(-.*\\)d\.:" date-str))

(defun rt/is-pending-deadline (date-str)
  (string-match "In \\([^-]*\\)d\.:" date-str))

(defun rt/is-deadline (date-str)
  (or (rt/is-due-deadline date-str)
	  (rt/is-late-deadline date-str)
	  (rt/is-pending-deadline date-str)))

(defun rt/is-scheduled (date-str)
  (or (rt/is-scheduled-today date-str)
	  (rt/is-scheduled-late date-str)))

(defun rt/is-scheduled-today (date-str)
  (string-match "Scheduled:" date-str))

(defun rt/is-scheduled-late (date-str)
  (string-match "Sched\.\\(.*\\)x:" date-str))

(defun rt/prepare-meeting-notes ()
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

(defun rt/mark-next-parent-tasks-todo ()
  "Visit each parent task and change NEXT states to TODO"
  (let ((mystate (or (and (fboundp 'state)
						  state)
					 (nth 2 (org-heading-components)))))
	(when (equal mystate "NEXT")
	  (save-excursion
		(while (org-up-heading-safe)
		  (when (member (nth 2 (org-heading-components)) (list "NEXT"))
			(org-todo "TODO")))))))
