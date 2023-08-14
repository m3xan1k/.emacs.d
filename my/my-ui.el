;; font
(set-face-attribute 'default nil :font "Ricty Diminished" :height 200)

;; themes
(use-package almost-mono-themes
  :ensure t
  :config
  ;; (load-theme 'almost-mono-gray t)
  (load-theme 'almost-mono-cream t))

;; modeline
(use-package doom-modeline
  :init
  (doom-modeline-mode 1))

;; The maximum displayed length of the branch name of version control.
(setq doom-modeline-vcs-max-length 32)

(use-package centaur-tabs
  :demand
  :config
  (centaur-tabs-mode t)
  :bind
  ("C-h" . centaur-tabs-backward)
  ("C-l" . centaur-tabs-forward))

(add-hook 'dired-mode-hook 'centaur-tabs-local-mode)

(centaur-tabs-headline-match)
(setq centaur-tabs-style "bar")
(setq centaur-tabs-plain-icons t)
(setq centaur-tabs-set-bar 'left)
(setq centaur-tabs-height 22)
(setq centaur-tabs-show-new-tab-button t)
(setq centaur-tabs-set-modified-marker t)
(setq centaur-tabs-cycle-scope 'tabs)
(centaur-tabs-group-by-projectile-project)
(centaur-tabs-change-fonts "Ricty Diminished" 120)

(defadvice vc-git-mode-line-string (after plus-minus (file) compile activate)
  "Show the information of git diff on modeline."
  (setq ad-return-value
	(concat (propertize ad-return-value 'face '(:foreground "black" :weight bold))
		" ["
		(let ((plus-minus (vc-git--run-command-string
				   file "diff" "--numstat" "--")))
		  (if (and plus-minus
		       (string-match "^\\([0-9]+\\)\t\\([0-9]+\\)\t" plus-minus))
		       (concat
			(propertize (format "+%s" (match-string 1 plus-minus)) 'face '(:foreground "green3"))
			(propertize (format "-%s" (match-string 2 plus-minus)) 'face '(:inherit font-lock-warning-face)))
		    (propertize "✔" 'face '(:foreground "green3" :weight bold))))
		"]")))

(use-package scroll-on-jump
  :config
  (setq scroll-on-jump-duration 1.0)
  (setq scroll-on-jump-smooth t))

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
  (scroll-on-jump-advice-add diff-hl-next-hunk)
  (scroll-on-jump-advice-add diff-hl-previous-hunk)
  (scroll-on-jump-with-scroll-advice-add evil-goto-line)
  (scroll-on-jump-with-scroll-advice-add evil-scroll-down)
  (scroll-on-jump-with-scroll-advice-add evil-scroll-up)
  (scroll-on-jump-with-scroll-advice-add evil-scroll-line-to-center)
  (scroll-on-jump-with-scroll-advice-add evil-scroll-line-to-top)
  (scroll-on-jump-with-scroll-advice-add evil-scroll-line-to-bottom))

(with-eval-after-load 'goto-chg
  (scroll-on-jump-advice-add goto-last-change)
  (scroll-on-jump-advice-add goto-last-change-reverse))

(provide 'my-ui)
