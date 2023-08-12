;; which key
(use-package which-key
  :config (which-key-mode))

(setq which-key-idle-delay 0.5)
(setq which-key-idle-secondary-delay 0)


;; git diff
(use-package diff-hl)
(global-diff-hl-mode)
(diff-hl-flydiff-mode 1)

(use-package blamer
  :defer 20
  :custom
  (blamer-idle-time 0.3)
  (blamer-min-offset 10)
  :custom-face
  (blamer-face ((t :foreground "#999999"
                    :background nil
                    :height 140
                    :italic nil)))
  :config
  (global-blamer-mode 1))

;; parens
(use-package smartparens
  :config
  (require 'smartparens-config)
  :hook
  ((clojure-mode emacs-lisp-mode) . smartparens-mode))

;; save sessions
(use-package desktop+)

;; same as vim-surround
(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))

(provide 'my-tools)
