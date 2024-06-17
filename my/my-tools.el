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
(use-package diff-hl)

(add-hook 'diff-hl-mode-on-hook
          (lambda ()
            (unless (window-system)
              (diff-hl-margin-local-mode))))

(global-diff-hl-mode)

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
  (setq neo-theme 'arrows))

;; scroll
(use-package scroll-on-jump
  :config
  (setq scroll-on-jump-duration 0.6
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

(scroll-on-jump-advice-add forward-paragraph)
(scroll-on-jump-advice-add backward-paragraph)
(scroll-on-jump-advice-add beginning-of-buffer)
(scroll-on-jump-advice-add end-of-buffer)
(scroll-on-jump-with-scroll-advice-add scroll-down-command)
(scroll-on-jump-with-scroll-advice-add scroll-up-command)

(with-eval-after-load 'goto-chg
  (scroll-on-jump-advice-add goto-last-change)
  (scroll-on-jump-advice-add goto-last-change-reverse))

;; http for my packages
(use-package request)

;; Show vi-like tilde in the fringe on empty lines.
(use-package vi-tilde-fringe
  :config
  (global-vi-tilde-fringe-mode 1))

;; pdf settings
(use-package pdf-tools
  :mode
  (("\\.pdf$" . pdf-view-mode)))

(setq pdf-view-use-scaling t)

;; insert current file name
(defun m3xan1k-get-file-name ()
  (interactive)
  (let ((filename (if (y-or-n-p "Absolute?")
                      buffer-file-name
                    (replace-regexp-in-string (projectile-project-root) "" buffer-file-name))))
    (kill-new filename)
    (message "Filename: %s is copied to clipboard." filename)))

(defun m3xan1k-region-to-another-file ()
  "Copies selected region to selected file"
  (interactive)
  (when (use-region-p)
    (kill-ring-save (region-beginning) (region-end))
    (find-file (read-file-name "Pick a file: "))
    (goto-char (point-max))
    (insert "\n")
    (yank)))

;; open url in firefox
(defun m3xan1k-open-in-firefox ()
  (interactive)
  (eshell-command (format "firefox %s" (thing-at-point 'url))))

(defun m3xan1k-jump-to-char ()
  (interactive)
  (let ((ch (read-char "Jump to char: ")))

    ;; to search next occurance
    (when (eq ch (char-after))
      (forward-char))

    (search-forward (char-to-string ch) (line-end-position))
    (backward-char)))

;; reopen closed buffer(only if file exists)
(setq m3xan1k-killed-file-list nil)

(defun m3xan1k-add-file-to-killed-file-list ()
  (when buffer-file-name
    (push buffer-file-name m3xan1k-killed-file-list)))

(add-hook 'kill-buffer-hook #'m3xan1k-add-file-to-killed-file-list)

(defun m3xan1k-reopen-killed-file ()
  (interactive)
  (when m3xan1k-killed-file-list
    (find-file (pop m3xan1k-killed-file-list))))

(defun m3xan1k-diff-to-branch ()
  (interactive)
  (let ((branch (read-string "Diff to branch: ")))
    (vc-root-version-diff (vc-root-dir) branch nil)))

;; keycast
;; (use-package keycast
;;   :config
;;   (keycast-mode-line-mode))

;; profile
;; (use-package esup)

(provide 'my-tools)
