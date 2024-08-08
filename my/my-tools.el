;; which key
(use-package which-key
  :defer 5
  :config
  (which-key-mode))

(setq which-key-idle-delay 0.5)
(setq which-key-idle-secondary-delay 0)
(setq which-key-max-display-columns 5)
(setq which-key-add-column-padding 3)

;; git gutter
(use-package git-gutter
  :defer 5
  :config
  (set-face-background 'git-gutter:modified "orange")
  (set-face-foreground 'git-gutter:modified "orange")
  (set-face-background 'git-gutter:added "green")
  (set-face-foreground 'git-gutter:added "green")
  (set-face-background 'git-gutter:deleted "red")
  (set-face-foreground 'git-gutter:deleted "red"))

(global-git-gutter-mode t)

;; parens
(use-package smartparens
  :commands smartparens-mode
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
;; (use-package magit
;;   :commands magit)

;; for my packages
(use-package esxml
  :defer 5)

;; file management
(use-package neotree
  :commands neotree-toggle
  :config
  (setq neo-theme 'arrows))

(defun neotree-project-dir ()
  "Open NeoTree using the git root."
  (interactive)
  (let ((project-dir (project-root (project-current)))
        (file-name (buffer-file-name)))
    (neotree-toggle)
    (if project-dir
        (if (neo-global--window-exists-p)
            (progn
              (neotree-dir project-dir)
              (neotree-find file-name)))
      (message "Could not find git project root."))))

;; highlight todos in source code
(use-package hl-todo
  :defer 5
  :init
  (global-hl-todo-mode)
  :config
  (setq hl-todo-keyword-faces
	'(("TODO"   . "#FF0000")
          ("FIXME"  . "#FF0000")
          ("DEBUG"  . "#A020F0")
          ("GOTCHA" . "#FF4500")
          ("STUB"   . "#1E90FF"))))

;; scroll
(use-package scroll-on-jump
  :defer 5
  :config
  (setq scroll-on-jump-duration 0.6
	scroll-on-jump-curve-power 2.0
	scroll-on-jump-curve 'linear))

(scroll-on-jump-advice-add forward-paragraph)
(scroll-on-jump-advice-add backward-paragraph)
(scroll-on-jump-advice-add beginning-of-buffer)
(scroll-on-jump-advice-add end-of-buffer)
(scroll-on-jump-with-scroll-advice-add scroll-down-command)
(scroll-on-jump-with-scroll-advice-add scroll-up-command)
(scroll-on-jump-with-scroll-advice-add recenter-top-bottom)

;; http for my packages
(use-package request
  :defer 5)

;; Show vi-like tilde in the fringe on empty lines.
(use-package vi-tilde-fringe
  :defer 5
  :config
  (global-vi-tilde-fringe-mode 1))

;; terminal
(use-package vterm
  :commands vterm)

;; pdf settings
;; (use-package pdf-tools
;;   :mode
;;   (("\\.pdf$" . pdf-view-mode)))

;; (setq pdf-view-use-scaling t)

;; keycast
;; (use-package keycast
;;   :config
;;   (keycast-mode-line-mode))

;; profile
;; (use-package esup)

(provide 'my-tools)
