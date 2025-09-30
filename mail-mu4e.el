(use-package dash
  :ensure dash-functional
  :config
  (dash-enable-font-lock))

(use-package mu4e
  :load-path "/usr/local/share/emacs/site-lisp/mu4e"
  :commands mu4e
  :bind (("\C-c\C-u" . mu4e-update-index)
		 ("\C-c\C-s" . mu4e-headers-change-sorting)
		 ([f1] . mu4e-in-new-frame))
  :config

  ;;need this for hash access
  (require 'subr-x)
  (require 'mu4e-actions)

  ;; use imagemagick, if available
  (when (fboundp 'imagemagick-register-types)
	(imagemagick-register-types))

  ;; message view action
  (defun mu4e-msgv-action-view-in-browser (msg)
	"View the body of the message in a web browser."
	(interactive)
	(let ((html (mu4e-message-field msg :body-html))
		  (txt (mu4e-message-field msg :body-txt))
		  (tmpfile (format "/tmp/%d.html" (random))))
	  (unless (or html txt)
		(mu4e-error "No body part for this message"))
	  (with-temp-buffer
		;; simplistic -- but note that it's only an example...
		(insert
		 (or
		  (concat "<html><head><meta http-equiv=\"content-type\" content=\"text/html;charset=UTF-8\">" html)
		  (concat "<pre>" txt "</pre>")))
		(write-file tmpfile)
		(browse-url-of-buffer-with-firefox (concat "file://" tmpfile)))))

  ;; when you reply to a message, use the identity that the mail was sent to
  ;; the cpbotha variation -- function that checks to, cc and bcc fields
  (defun cpb-mu4e-is-message-to (msg rx)
	"Check if to, cc or bcc field in MSG has any address in RX."
	(or (mu4e-message-contact-field-matches msg :to rx)
		(mu4e-message-contact-field-matches msg :cc rx)
		(mu4e-message-contact-field-matches msg :bcc rx)))

  (setq contact-file "/home/i02sopop/org/contacts.txt")
  (defun read-contact-list ()
	"Return a list of email addresses"
	(with-temp-buffer
	  (insert-file-contents contact-file)
	  (split-string (buffer-string) "\n" t)))

  ;; code from https://github.com/abo-abo/swiper/issues/596
  (defun counsel-email-action (contact)
	(with-ivy-window
	 (insert contact)))

  ;; bind comma to launch new search
  (defvar counsel-email-map
	(let ((map (make-sparse-keymap)))
	  (define-key map "," 'counsel-email-more)
	  map))

  (defun counsel-email-more ()
	"Insert email address and prompt for another."
	(interactive)
	(ivy-call)
	(with-ivy-window
	 (insert ", "))
	(delete-minibuffer-contents)
	(setq ivy-text ""))

  ;; ivy contacts
  ;; based on http://kitchingroup.cheme.cmu.edu/blog/2015/03/14/A-helm-mu4e-contact-selector/
  (defun ivy-select-and-insert-contact (&optional start)
	(interactive)
	;; make sure mu4e contacts list is updated - I was having
	;; intermittent problems that this was empty but couldn't see why
	(mu4e~request-contacts)
	(let ((eoh ;; end-of-headers
		   (save-excursion
			 (goto-char (point-min))
			 (search-forward-regexp mail-header-separator nil t)))
		  ;; append full sorted contacts list to favourites and delete duplicates
		  (contacts-list
		   (delq nil (delete-dups (append (read-contact-list)
										  (mu4e~sort-contacts-for-completion
										   (hash-table-keys mu4e~contacts)))))))

	  ;; only run if we are in the headers section
	  (when (and eoh (> eoh (point)) (mail-abbrev-in-expansion-header-p))
		(let* ((end (point))
			   (start
				(or start
					(save-excursion
					  (re-search-backward "\\(\\`\\|[\n:,]\\)[ \t]*")
					  (goto-char (match-end 0))
					  (point))))
			   (initial-input (buffer-substring-no-properties start end)))

		  (delete-region start end)

		  (ivy-read "Contact: "
					contacts-list
					:re-builder #'ivy--regex
					:sort nil
					:initial-input initial-input
					:action 'counsel-email-action
					:keymap counsel-email-map)
		  ))))

  ;; we only do something if we recognize something (i.e. no stupid default)
  (add-hook 'mu4e-compose-pre-hook
			(defun my-set-from-address ()
			  "Set current identity based on to, cc, bcc of original."
			  (let ((msg mu4e-compose-parent-message)) ;; msg is shorter...
				(if msg
					(cond
					 ((cpb-mu4e-is-message-to msg (list "palvarez@ritho.net"))
					  (setq  user-mail-address "palvarez@ritho.net"))
					 ((cpb-mu4e-is-message-to msg (list "ritho@ritho.net"))
					  (setq  user-mail-address "ritho@ritho.net"))
					 ((cpb-mu4e-is-message-to msg (list "palvarez@ritho.es"))
					  (setq  user-mail-address "palvarez@ritho.es"))
					 ((cpb-mu4e-is-message-to msg (list "ritho@ritho.es"))
					  (setq  user-mail-address "ritho@ritho.es"))
					 ((cpb-mu4e-is-message-to msg (list "i02sopop@ritho.es"))
					  (setq  user-mail-address "i02sopop@ritho.es"))
					 ((cpb-mu4e-is-message-to msg "i02sopop@gmail.com")
					  (setq  user-mail-address "i02sopop@gmail.com"))
					 ((cpb-mu4e-is-message-to msg "palvarez@vintagram.es")
					  (setq  user-mail-address "palvarez@vintagram.es"))
					 ((cpb-mu4e-is-message-to msg "i02sopop@ritho.net")
					  (setq  user-mail-address "i02sopop@ritho.net")))))))

  (add-hook 'mu4e-compose-mode-hook
  (defun my-do-compose-stuff ()
    "My settings for message composition."
    (set-fill-column 80)
    (flyspell-mode)
	(setq indent-line-function 'indent-relative)))

  ;; convenience function for starting the whole mu4e in its own frame
  ;; posted by the author of mu4e on the mailing list
  (defun mu4e-in-new-frame ()
	"Start mu4e in new frame."
	(interactive)
	(select-frame (make-frame))
	(mu4e))

  ;; Email
  ;; path to our Maildir directory
  (setq mu4e-maildir (expand-file-name "~/.local/share/local-mail"))
  (setq mu4e-cache-maildir-list 't)
  (setq mu4e-attachment-dir  "~/Downloads")
  (setq mu4e-headers-results-limit 3000)

  ;; Modify the expression introducing a quoted email:
  (setq message-citation-line-function 'message-insert-formatted-citation-line)
  (setq message-citation-line-format "On %d %b %Y, at %T (%Z), %f wrote:\n")

  ;; the next are relative to `mu4e-maildir'
  ;; instead of strings, they can be functions too, see
  ;; their docstring or the chapter 'Dynamic folders'
  (setq mu4e-sent-folder   "/sent-mail"
		mu4e-drafts-folder "/drafts"
		mu4e-trash-folder  "/trash"
		mu4e-refile-folder "/archive")

  (setq mu4e-allow-edit-any-message 't)
  (setq mu4e-auto-retrieve-keys 't)
  (setq mu4e-compose-auto-include-date 't)
  (setq mu4e-compose-dont-reply-to-self 't)
  (setq mu4e-compose-signature
		"Pablo Álvarez de Sotomayor Posadillo
 Bachelor Degree in Computer Science
           http://ritho.net
    http://ritho.net/blog
    \"Of all the things I've lost,
      I miss my mind the most.\"")

  (setq mu4e-user-mail-address-list
		'("palvarez@ritho.net"
		  "i02sopop@gmail.com"
		  "palvarez@vintagram.es"
		  "i02sopop@ritho.net"
		  "ritho@ritho.net"
		  "palvarez@kirinki.net"
		  "palvarez@kirinki.com"
		  "palvarez@kirinki.org"
		  "palvarez@kirinki.es"
		  "palvarez@kirinki.eu"
		  "palvarez@linuxcordoba.org"
		  "ritho@linuxcordoba.org"
		  "palvarez@ritholution.com"
		  "ritho@ritholution.com"
		  "palvarez@ritholution.es"
		  "ritho@ritholution.es"
		  "palvarez@ritholution.eu"
		  "ritho@ritholution.eu"
		  "palvarez@ritho.es"
		  "ritho@ritho.es"
		  "i02sopop@ritho.es"))

  (setq user-mail-address "palvarez@ritho.net"
		user-full-name  "Pablo Alvarez de Sotomayor Posadillo")

  (setq mu4e-view-show-addresses 't)
  (setq mu4e-view-show-images 't)
  (setq mu4e-view-image-max-width '1920)
  (setq mu4e-view-image-max-height '1080)
  (setq mu4e-save-multiple-attachments-without-asking 't)
  (setq mu4e-maildir-shortcuts '((:maildir "/archive" :key ?a)
								 (:maildir "/rss/slashdot" :key ?d)
								 (:maildir "/rss/hacker-news" :key ?h)
								 (:maildir "/rss/arstechnica" :key ?A)
								 (:maildir "/entrada" :key ?i)
								 (:maildir "/rss/lwn" :key ?l)
								 (:maildir "/rss/meneame" :key ?m)
								 (:maildir "/acronis" :key ?w)
								 (:maildir "/rehuerta" :key ?r)
								 (:maildir "/sent-mail" :key ?s)))

  (setq mu4e-maildirs '("/acronis"
						"/aula-linux"
						"/autoconf"
						"/automake"
						"/automake-commit"
						"/automake-patches"
						"/bug-autogen"
						"/bug-bison"
						"/bug-findutils"
						"/bug-gettext"
						"/bug-glibc"
						"/bug-global"
						"/bug-gnu-emacs"
						"/bug-gnu-utils"
						"/bug-grep"
						"/bug-inetutils"
						"/bug-mailutils"
						"/bug-make"
						"/build-inetutils"
						"/bulmages"
						"/carrera"
						"/colinaroja"
						"/contabilidad"
						"/coreutils"
						"/coreutils-announce"
						"/courses"
						"/cuentas"
						"/david-kadavy"
						"/ddd"
						"/debian-announce"
						"/debian-changes"
						"/debian-devel"
						"/debian-devel-announce"
						"/debian-devel-spanish"
						"/debian-i18n"
						"/debian-jobs"
						"/debian-mentors"
						"/debian-newmaint"
						"/debian-news"
						"/debian-news-spanish"
						"/debian-policy"
						"/debian-project"
						"/debian-qa"
						"/debian-qa-packages"
						"/debian-release"
						"/debian-security"
						"/debian-security-announce"
						"/debian-security-tracker"
						"/debian-vote"
						"/debian-women"
						"/dicussion-fsfla"
						"/discussion-fsfe"
						"/docuforum"
						"/drafts"
						"/emacs-devel"
						"/epla"
						"/estandares-abiertos"
						"/familia"
						"/fb.ie"
						"/ffii"
						"/ffii-latin"
						"/findutils-patches"
						"/friends"
						"/fsfe"
						"/fsfe-es"
						"/full-disclosure"
						"/g1-hackers"
						"/gcubo-anuncios"
						"/gcubo-ayuda"
						"/gcubo-cicode"
						"/gcubo-general"
						"/gdb"
						"/github/repos"
						"/github/notifications"
						"/god/announce"
						"/god/devel"
						"/god/patches"
						"/glibc-bugs"
						"/gnu-c"
						"/gnu-emacs-sources"
						"/gnu-system-discuss"
						"/grep-commit"
						"/hacklabs"
						"/hackmeeting"
						"/help-bison"
						"/help-gnutls"
						"/help-gnu-utils"
						"/home"
						"/indy-cordoba"
						"/INET"
						"/info-gnu-emacs"
						"/info-gnu-events"
						"/lana"
						"/lana/chile"
						"/lana/code_review"
						"/lana/infrastructure"
						"/lana/mexico"
						"/lana/notion"
						"/lana/payfit"
						"/lana/rfc"
						"/lana/trello"
						"/laura_ribas"
						"/libav-devel"
						"/libc-alpha"
						"/libc-announce"
						"/libc-help"
						"/libc-ports"
						"/licor"
						"/licor-anuncios"
						"/licor-socios"
						"/licor-usuarios"
						"/linkedin"
						"/m4h-general"
						"/mailman-announces"
						"/mailman-coders"
						"/mailman-developers"
						"/make-alpha"
						"/make-commits"
						"/minix"
						"/n-1"
						"/news"
						"/nextgen"
						"/openstandards"
						"/opentia"
						"/Oreilly"
						"/pabellon"
						"/patents"
						"/pdf-devel"
						"/phabricator-dev"
						"/press-release"
						"/press-release-es"
						"/psql-hackers"
						"/red_cordoba"
						"/redis-db"
						"/republica"
						"/ritholution"
						"/ritho.net"
						"/rss"
						"/rss/adam_nuttall"
						"/rss/aldea"
						"/rss/antirez"
						"/rss/arstechnica"
						"/rss/barrapunto"
						"/rss/bulmages"
						"/rss/clean_coder"
						"/rss/code_and_beyond"
						"/rss/dcordero"
						"/rss/debian_sysadmins"
						"/rss/desencadenado"
						"/rss/dtrace"
						"/rss/engadget"
						"/rss/golang-news"
						"/rss/go-blog"
						"/rss/go-dave-cheney"
						"/rss/google_testing"
						"/rss/howto_geek"
						"/rss/kernel_planet"
						"/rss/kriptopolis"
						"/rss/kubernetes"
						"/rss/linux-adictos"
						"/rss/linux_admin_show"
						"/rss/linux_magazine"
						"/rss/maddog"
						"/rss/malaprensa"
						"/rss/medialab"
						"/rss/microsiervos"
						"/rss/music_for_programming"
						"/rss/nba"
						"/rss/neeraj"
						"/rss/pornohardware"
						"/rss/postgres"
						"/rss/productivity_sauce"
						"/rss/security_art"
						"/rss/se-radio"
						"/rss/superhabitos"
						"/rss/xkcd"
						"/SAML"
						"/Schibsted"
						"/Schibsted/Bikhir"
						"/Schibsted/chorradas"
						"/Schibsted/CustoJusto"
						"/Schibsted/dev"
						"/Schibsted/Jofogas"
						"/Schibsted/Kapaza"
						"/Schibsted/Keywi"
						"/Schibsted/Nominas"
						"/Schibsted/Platform"
						"/Schibsted/SNT"
						"/Schibsted/Vacaciones"
						"/Schibsted/Yapo"
						"/Segundamano MX"
						"/SMMX"
						"/spam"
						"/squirrelmail-devel"
						"/system-hackers"
						"/templates"
						"/Tori"
						"/trabajo"
						"/tramp-devel"
						"/trash"
						"/viajes"
						"/vintagram"
						"/vintagram-commits"
						"/vintagram-junta"
						"/wsis-euc"
						"/wsis-pct"
						"/wsis-sst"
						"/zaleos"))

  ;; don't keep message buffers around
  (setq message-kill-buffer-on-exit 't)

  ;; use 'fancy' non-ascii characters in various places in mu4e
  (setq mu4e-use-fancy-chars nil)

  ;; This is needed to allow msmtp to do its magic:
  (setq message-sendmail-f-is-evil 't)

  ;; tell message-mode how to send mail
  (setq message-send-mail-function 'message-send-mail-with-sendmail)
  (setq sendmail-program "~/bin/msmtp-enqueue")

  (setq mu4e-get-mail-command "fetchmail -v")

  (define-key mu4e-view-mode-map "f" 'browse-url-of-buffer-with-firefox)
  (define-key mu4e-view-mode-map "l" 'el-pocket-add-url)
  (define-key mu4e-main-mode-map (kbd "<f1>") 'cpb-mu4e-palvarez-ritho)
  (define-key mu4e-main-mode-map (kbd "<f2>") 'cpb-mu4e-i02sopop-gmail)
  (define-key mu4e-main-mode-map (kbd "<f3>") 'cpb-mu4e-palvarez-vintagram)
  (define-key mu4e-main-mode-map (kbd "<f4>") 'cpb-mu4e-i02sopop-ritho)

  (add-to-list 'mu4e-view-actions '("View in browser" . mu4e-msgv-action-view-in-browser) t)
  (add-to-list 'mu4e-view-actions '("Apply mbox" . mu4e-action-git-apply-mbox) t)

  (defun sh-patchset-update (status)
	(save-excursion
      (goto-char (point-min))
	  (kill-matching-lines "^X-Sourcehut-Patchset-Update: .*$")
	  (let ((end-header
			 (re-search-forward "^--text follows this line--[[:space:]]*$" nil t)))
		(when end-header
		  (previous-logical-line)
		  (insert "X-Sourcehut-Patchset-Update: " status "\n")
		  ))
	  ))

  (defun sh-patchset-needs-revision ()
	"Sourcehut patchset needs revision"
	(interactive)
	(sh-patchset-update "NEEDS_REVISION"))

  (defun sh-patchset-superseded ()
	"Sourcehut patchset superseded"
	(interactive)
	(sh-patchset-update "SUPERSEDED"))

  (defun sh-patchset-approved ()
	"Sourcehut patchset approved"
	(interactive)
	(sh-patchset-update "APPROVED"))

  (defun sh-patchset-proposed ()
	"Sourcehut patchset proposed"
	(interactive)
	(sh-patchset-update "PROPOSED"))

  (defun sh-patchset-rejected ()
	"Sourcehut patchset rejected"
	(interactive)
	(sh-patchset-update "REJECTED"))

  (defun sh-patchset-applied ()
	"Sourcehut patchset applied"
	(interactive)
	(sh-patchset-update "APPLIED"))

  (define-key mu4e-compose-mode-map (kbd "C-c p p") #'sh-patchset-proposed)
  (define-key mu4e-compose-mode-map (kbd "C-c p a") #'sh-patchset-approved)
  (define-key mu4e-compose-mode-map (kbd "C-c p r") #'sh-patchset-rejected)
  (define-key mu4e-compose-mode-map (kbd "C-c p s") #'sh-patchset-superseded)
  (define-key mu4e-compose-mode-map (kbd "C-c p n") #'sh-patchset-needs-revision)
  (define-key mu4e-compose-mode-map (kbd "C-c p m") #'sh-patchset-applied)
) ;; end of use-package

(setq mail-user-agent 'mu4e-user-agent)
