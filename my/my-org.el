;; org defaults
(require 'org)

(defun setup-org-defaults ()
  (setq-local word-wrap t)
  (setq-local fill-column 70)
  (auto-fill-mode t)
  (visual-line-mode t))

(add-hook 'org-mode-hook #'setup-org-defaults)

;; bullets for better visibility
(use-package org-bullets
  :hook
  (org-mode . org-bullets-mode))

(setq org-return-follows-link t)
(setq org-hide-emphasis-markers t)

;; for org-capture notes
(setq org-directory "~/Nextcloud/org")

(setq org-capture-templates
      '(("w" "Work" entry (file+datetree "~/Nextcloud/org/capture/work.org")
         "* %?\nEntered on %U\n  %i\n  %a")
        ("t" "Tech" entry (file+headline "~/Nextcloud/org/capture/tech.org" "Tech")
         "* %?\nEntered on %U\n  %i\n  %a")
        ("p" "Personal" entry (file+headline "~/Nextcloud/org/capture/personal.org" "Personal")
         "* %?\nEntered on %U\n  %i\n  %a")
        ("j" "Journal" entry (file+datetree "~/org/journal.org")
         "* %?\nEntered on %U\n  %i\n  %a")))

;; todos path for agenda
(setq org-agenda-files '("~/Nextcloud/org/agenda"))
(setq org-agenda-inhibit-startup t)
(setq org-agenda-skip-deadline-if-done t)
(setq org-agenda-skip-scheduled-if-done t)

;; some defaults
(setq org-adapt-indentation t)
(setq org-todo-keywords
      '((sequence
	 "NEXT(n)"
	 "TODO(t)"
	 "IN PROGRESS(i)"
	 "WAITING(w)"
	 "SOMEDAY(s)"
	 "PROJ(p)"
	 "REPEAT(r)"
	 "|"
	 "DONE(d)"
	 "CANCELLED(c)")))

;; custom template
(with-eval-after-load "org"
  (add-to-list 'org-structure-template-alist
	       '("my"
		 .
		 "#+TITLE:\n#+TAGS: PHONE(o) COMPUTER(c) SHOPPING(s) URGENT(u)\n#+SEQ_TODO: NEXT(n) TODO(t) WAITING(w) SOMEDAY(s) PROJ(p) INPROGRESS(i) REPEAT(r) | DONE(d) CANCELLED(c)\n#+STARTUP: nologrepeat\n")))

;; for timer
(setq org-clock-sound "~/Nextcloud/music/bell.wav")

(use-package denote)
(setq denote-directory (expand-file-name "~/Nextcloud/denote/"))
(setq denote-org-store-link-to-heading t)

(provide 'my-org)
