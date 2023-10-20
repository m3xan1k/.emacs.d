;; org
(setq org-adapt-indentation t)
(setq org-agenda-files '("~/Nextcloud/org/src/todos" "~/Downloads/orgmode-coursefiles/sec-2.4-start-mylife.org"))
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

(use-package org-bullets
  :hook
  (org-mode . org-bullets-mode))

(provide 'my-org)