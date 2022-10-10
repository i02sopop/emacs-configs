(use-package tex
  :ensure auctex
  :config
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq-default TeX-master nil)

  (add-to-list 'org-latex-classes
			   '("report-noparts"
				 "\\documentclass{report}"
				 ("\\chapter{%s}" . "\\chapter*{%s}")
				 ("\\section{%s}" . "\\section*{%s}")
				 ("\\subsection{%s}" . "\\subsection*{%s}")
				 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
				 ("\\paragraph{%s}" . "\\paragraph*{%s}")
				 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

  (add-to-list 'org-latex-classes
			   '("book-noparts"
				 "\\documentclass{book}"
				 ("\\chapter{%s}" . "\\chapter*{%s}")
				 ("\\section{%s}" . "\\section*{%s}")
				 ("\\subsection{%s}" . "\\subsection*{%s}")
				 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
				 ("\\paragraph{%s}" . "\\paragraph*{%s}")
				 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
  )

(use-package auctex-latexmk
  :ensure t
  :config
  (auctex-latexmk-setup)
  (setq auctex-latexmk-inherit-TeX-PDF-mode t)
  )

(use-package auto-complete-auctex
  :ensure t)

(use-package bibretrieve
  :ensure t)

(use-package bibslurp
  :ensure t)

(use-package bibtex-utils
  :ensure t
  :config
  (eval-after-load 'bibtex
	'(define-key bibtex-mode-map "\C-ck" 'bu-make-field-keywords)))

;; (use-package ebib
;;   :ensure t
;;   :config
;;   (global-set-key "\C-ce" 'ebib))

(use-package latex-extra
  :ensure t
  :config
  (add-hook 'LaTeX-mode-hook #'latex-extra-mode))
