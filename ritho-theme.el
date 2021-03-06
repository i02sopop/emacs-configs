(deftheme ritho
  "ritho")

(custom-theme-set-faces
 'ritho

 '(default ((t (:stipple nil :background "black" :foreground "green" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 120 :width normal :family "courier"))))
 '(bmkp-local-file-without-region ((t (:foreground "white"))))
 '(bold ((t (:weight bold))))
 '(bold-italic ((t (:slant italic :weight bold))))
 '(button ((t (:inherit link))))
 '(custom-comment-tag ((t (:foreground "green"))))
 '(custom-face-tag ((t (:inherit custom-variable-tag :foreground "green"))))
 '(custom-group-tag ((t (:inherit variable-pitch :foreground "green" :weight bold :height 1.2))))
 '(flymake-errline ((((class color)) (:underline "red"))))
 '(flymake-warnline ((((class color)) (:underline "yellow"))))
 '(header-line ((t (:inherit mode-line :background "green" :foreground "black"))))
 '(highlight ((t (:background "blue" :foreground "red"))))
 '(hl-line ((t (:inherit highlight :background "darkseagreen2"))))
 '(identica-highlight-face ((t (:background "SlateGray"))))
 '(info-file ((t (:background "DimGray" :foreground "yellow"))))
 '(lazy-highlight ((t (:background "blue" :foreground "red"))))
 '(link ((t (:foreground "yellow" :underline t))))
 '(minibuffer-prompt ((t (:foreground "yellow"))))
 '(mode-line ((t (:background "black" :foreground "green" :inverse-video t))))
 '(mode-line-buffer-id ((t (:inherit bold :background "red"))))
 '(mode-line-emphasis ((t (:background "blue" :foreground "yellow"))))
 '(mode-line-highlight ((t (:inherit highlight :background "red" :foreground "yellow"))))
 '(mumamo-background-chunk-major ((((class color) (min-colors 8)) nil)))
 '(mumamo-background-chunk-submode1 ((((class color) (min-colors 8)) nil)))
 '(mumamo-background-chunk-submode2 ((((class color) (min-colors 8)) nil)))
 '(mumamo-background-chunk-submode3 ((((class color) (min-colors 8)) nil)))
 '(mumamo-background-chunk-submode4 ((((class color) (min-colors 8)) nil)))
 '(org-agenda-restriction-lock ((((class color) (min-colors 8)) (:background "green" :foreground "black"))))
 '(org-clock-overlay ((((class color) (min-colors 8)) (:background "green" :foreground "black"))))
 '(org-column ((((class color) (min-colors 8)) (:background "black" :foreground "green" :strike-through nil :underline nil :slant normal :weight normal))))
 '(org-column-title ((((class color) (min-colors 8)) (:background "black" :foreground "green" :underline t :weight bold))))
 '(org-habit-alert-face ((t nil)))
 '(org-habit-alert-future-face ((t nil)))
 '(org-habit-clear-face ((t nil)))
 '(org-habit-clear-future-face ((t nil)))
 '(org-habit-overdue-face ((t nil)))
 '(org-habit-overdue-future-face ((t nil)))
 '(org-habit-ready-face ((((background dark)) (:background "forestgreen"))))
 '(org-level-3 ((t (:inherit outline-3))))
 '(org-mode-line-clock ((t (:background "grey75" :foreground "red" :box (:line-width -1 :style released-button)))))
 '(secondary-selection ((t (:background "blue" :foreground "black"))))
 '(speedbar-directory-face ((t (:foreground "dark blue"))))
 '(speedbar-file-face ((t (:foreground "blue"))))
 '(isearch ((t (:background "lime green" :foreground "brown4"))))
 '(isearch-fail ((((class color) (min-colors 88) (background light)) (:background "RosyBrown1")) (((class color) (min-colors 88) (background dark)) (:background "red4")) (((class color) (min-colors 16)) (:background "red")) (((class color) (min-colors 8)) (:background "red")) (((class color grayscale)) (:foreground "grey")) (t (:inverse-video t))))
 '(isearch-secondary ((t nil))))

(provide-theme 'ritho)
