;; font
(set-face-attribute 'default nil :font "Input" :height 170)

;; themes
(use-package almost-mono-themes
  :ensure t
  :config
  (load-theme 'almost-mono-cream t))

;; tab bar
(use-package centaur-tabs
  :demand
  :config
  (centaur-tabs-mode t)
  (centaur-tabs-group-by-projectile-project)
  (centaur-tabs-change-fonts "Input" 120))

(setq centaur-tabs-cycle-scope 'tabs)
(setq centaur-tabs-set-modified-marker t)

;; disable in dired
(add-hook 'dired-mode-hook 'centaur-tabs-local-mode)

;; modeline
(use-package doom-modeline
  :init
  (doom-modeline-mode 1))

;; The maximum displayed length of the branch name of version control.
(setq doom-modeline-vcs-max-length 32)

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

(provide 'my-ui)
