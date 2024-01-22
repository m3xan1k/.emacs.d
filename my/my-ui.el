;; font
(set-face-attribute 'default nil :font "Liberation Mono" :height 165)
(set-face-attribute 'fringe nil :background nil)

;; additional theme
(use-package almost-mono-themes)

;; tabs
(use-package centaur-tabs
  :demand
  :config
  (centaur-tabs-mode t)
  (centaur-tabs-group-by-projectile-project)
  (centaur-tabs-change-fonts "Liberation Mono" 120))

(setq centaur-tabs-cycle-scope 'tabs)
(setq centaur-tabs-set-modified-marker t)

;; projectile for projects discovery
(use-package projectile
  :init
  (projectile-mode t))

;; The maximum displayed length of the branch name of version control.
(setq modeline-vcs-max-length 32)

;; git status customization
(defun my/format-git-diff (plus-minus)
  "Takes 1\t2 returns [+1-2]"
  (concat "["
	  (if (and plus-minus
		   (string-match "^\\([0-9]+\\)\t\\([0-9]+\\)\t" plus-minus))
	      (concat
	       (propertize (format "+%s" (match-string 1 plus-minus)))
	       (propertize (format "-%s" (match-string 2 plus-minus))))
	    (propertize "✔" 'face '(:weight bold)))
	  "]"))

(defun my/glue-branch-diff (branch-name plus-minus-formatted)
  "returns branch-name[diff]"
  (let ((cut-length (- modeline-vcs-max-length
		       (+ 2 (length plus-minus-formatted)))))
    (if (< cut-length (length branch-name))
	(concat (substring branch-name 0 cut-length)
		".."
		plus-minus-formatted)
      (concat branch-name plus-minus-formatted))))

(defadvice vc-git-mode-line-string (after plus-minus (file) compile activate)
  "Show the information of git diff on modeline."
  (let* ((plus-minus (vc-git--run-command-string file "diff" "--numstat" "--"))
	 (plus-minus-formatted (my/format-git-diff plus-minus)))
    (setq ad-return-value
	  (my/glue-branch-diff ad-return-value plus-minus-formatted))))

;; hide minor modes
(use-package minions
  :config
  (minions-mode t))

;; minimal modeline
(use-package mood-line
  :config
  (mood-line-mode))

;; scrollbar
(use-package yascroll)
(global-yascroll-bar-mode 1)

(provide 'my-ui)
