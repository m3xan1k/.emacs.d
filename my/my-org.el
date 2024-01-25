;; org

;; bullets for better visibility
(use-package org-bullets
  :hook
  (org-mode . org-bullets-mode))

(use-package org-preview-html)

;; todos path for agenda
(setq org-agenda-files '("~/Nextcloud/org/src/todos"))
(setq org-agenda-inhibit-startup t)

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

(provide 'my-org)
