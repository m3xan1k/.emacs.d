;; which key
(use-package which-key
  :config
  (which-key-mode))

(setq which-key-idle-delay 0.5)
(setq which-key-idle-secondary-delay 0)
(setq which-key-max-display-columns 5)
(setq which-key-add-column-padding 3)

;; git gutter
(use-package git-gutter)

;; If you enable global minor mode
(global-git-gutter-mode t)

;; parens
(use-package smartparens
  :config
  (require 'smartparens-config))

(add-hook 'prog-mode-hook #'smartparens-mode)

;; same as vim-surround
(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))

;; start page
(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook))

(setq dashboard-projects-backend 'project-el)
(setq dashboard-items '((recents  . 5)
                        (projects . 5)
                        (agenda . 5)))

;; terminal clipboard
(use-package xclip
  :init
  (xclip-mode))

;; restclient
(use-package restclient)

;; magit
(use-package magit)

;; profile
;; (use-package esup)

(provide 'my-tools)
