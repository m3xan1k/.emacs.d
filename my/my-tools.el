;; which key
(use-package which-key
  :config
  (which-key-mode))

(setq which-key-idle-delay 0.5)
(setq which-key-idle-secondary-delay 0)
(setq which-key-max-display-columns 5)
(setq which-key-add-column-padding 3)

;; git diff
(use-package diff-hl)
(global-diff-hl-mode)
(diff-hl-flydiff-mode 1)

(add-hook 'diff-hl-mode-on-hook
          (lambda ()
            (unless (window-system)
              (diff-hl-margin-local-mode))))

;; parens
(use-package smartparens
  :config
  (require 'smartparens-config))

(add-hook 'prog-mode-hook #'smartparens-mode)

;; save sessions
(use-package desktop+)

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

(setq dashboard-items '((recents  . 5)
                        (projects . 5)
                        (agenda . 5)))

;; terminal clipboard
(use-package xclip
  :init
  (xclip-mode))

(provide 'my-tools)
