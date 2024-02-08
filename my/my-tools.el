;; projectile for projects discovery
(use-package projectile
  :init
  (projectile-mode t))

;; projectile will list projects from these paths
(setq projectile-project-search-path
      '(("~/Documents/projects/" . 1) ("~/Documents/SPELL/" . 1) ("~/.emacs.d/" . 1)))

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

;; start page
(use-package dashboard
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

;; magit
(use-package magit)

;; for my packages
(use-package esxml)

;; file management
(use-package neotree
  :config
  (setq projectile-switch-project-action 'neotree-projectile-action))

;; scroll
(use-package scroll-on-jump
  :config
  (setq scroll-on-jump-duration 0.7
	scroll-on-jump-curve-power 2.0
	scroll-on-jump-curve 'linear))

(with-eval-after-load 'evil
  (scroll-on-jump-advice-add evil-undo)
  (scroll-on-jump-advice-add evil-redo)
  (scroll-on-jump-advice-add evil-jump-item)
  (scroll-on-jump-advice-add evil-jump-forward)
  (scroll-on-jump-advice-add evil-jump-backward)
  (scroll-on-jump-advice-add evil-ex-search-next)
  (scroll-on-jump-advice-add evil-ex-search-previous)
  (scroll-on-jump-advice-add evil-forward-paragraph)
  (scroll-on-jump-advice-add evil-backward-paragraph)
  (scroll-on-jump-advice-add evil-goto-mark)

  ;; Actions that themselves scroll.
  (scroll-on-jump-with-scroll-advice-add evil-goto-line)
  (scroll-on-jump-with-scroll-advice-add evil-scroll-down)
  (scroll-on-jump-with-scroll-advice-add evil-scroll-up)
  (scroll-on-jump-with-scroll-advice-add evil-scroll-line-to-center)
  (scroll-on-jump-with-scroll-advice-add evil-scroll-line-to-top)
  (scroll-on-jump-with-scroll-advice-add evil-scroll-line-to-bottom))

(with-eval-after-load 'goto-chg
  (scroll-on-jump-advice-add goto-last-change)
  (scroll-on-jump-advice-add goto-last-change-reverse))

;; keycast
;; (use-package keycast
;;   :config
;;   (keycast-mode-line-mode))

;; profile
;; (use-package esup)

(provide 'my-tools)
