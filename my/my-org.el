;; org

;; bullets for better visibility
(use-package org-bullets
  :hook
  (org-mode . org-bullets-mode))

(use-package org-preview-html)

;; for org-capture notes
(setq org-directory "~/Nextcloud/org/capture")

(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/Nextcloud/org/capture/gtd.org" "Tasks")
         "* TODO %?\n  %i\n  %a")
        ("s" "Spell" entry (file+datetree "~/Nextcloud/org/capture/spell.org")
         "* %?\nEntered on %U\n  %i\n  %a")
	("n" "Notes" entry (file+headline "~/Nextcloud/org/capture/notes.org" "Notes")
	 "* %?\n")))

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

(setq org-tag-alist
 '(("ADDRESS" . ?a)
   ("PHONE" . ?p)
   ("URGENT" . ?u)))

;; custom template
(with-eval-after-load "org"
  (add-to-list 'org-structure-template-alist
   '("my"
     .
     "#+TITLE:\n#+TAGS: PHONE(o) COMPUTER(c) SHOPPING(s) URGENT(u)\n#+SEQ_TODO: NEXT(n) TODO(t) WAITING(w) SOMEDAY(s) PROJ(p) INPROGRESS(i) REPEAT(r) | DONE(d) CANCELLED(c)\n#+STARTUP: nologrepeat\n")))

; (use-package org-roam
;   :ensure t
;   :init
;   (setq org-roam-v2-ack t)
;   :custom
;   (org-roam-directory "~/roam")
;   :bind (("C-c o t" . org-roam-buffer-toggle)
;          ("C-c o f" . org-roam-node-find)
;          ("C-c o i" . org-roam-node-insert))
;   :config
;   (org-roam-setup))

(provide 'my-org)
