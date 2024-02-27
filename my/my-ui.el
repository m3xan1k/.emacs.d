;; Theme
(load-theme 'tsdh-light)

;; slight modifications
(custom-theme-set-faces
 'tsdh-light
 '(font-lock-keyword-face ((t (:foreground "black" :weight bold))))
 '(font-lock-variable-name-face ((t (:foreground "black")))))

;; font @
(set-face-attribute 'default nil
		    :font "Dejavu Sans Mono"
		    :height 165
		    :background "#FDF6E3"
		    :foreground "#222222")
(set-face-attribute 'fringe nil
		    :background (face-background 'default)
		    :foreground (face-foreground 'default))
(set-face-attribute 'line-number-current-line nil
		    :background (face-background 'default)
		    :foreground (face-foreground 'default)
		    :weight 'bold)
(set-face-attribute 'line-number nil
                    :background (face-background 'default nil t))
(set-face-attribute 'cursor nil
		    :background "red")
(set-face-attribute 'font-lock-constant-face nil
		    :foreground "#black" :weight 'bold)

(set-face-background 'git-gutter:modified "orange")
(set-face-foreground 'git-gutter:modified "orange")
(set-face-background 'git-gutter:added "green")
(set-face-foreground 'git-gutter:added "green")
(set-face-background 'git-gutter:deleted "red")
(set-face-foreground 'git-gutter:deleted "red")

(set-cursor-color "red")

(setq-default line-spacing 3)

;; typography
(set-char-table-range char-width-table '(?— . ?—) 2)

;; The maximum displayed length of the branch name of version control.
(setq modeline-vcs-max-length 32)

;; git status customization
(defun m3xan1k/format-git-diff (plus-minus)
  "Takes 1\t2 returns [+1-2]"
  (concat "["
	  (if (and plus-minus
		   (string-match "^\\([0-9]+\\)\t\\([0-9]+\\)\t" plus-minus))
	      (concat
	       (propertize (format "+%s" (match-string 1 plus-minus)))
	       (propertize (format "-%s" (match-string 2 plus-minus))))
	    (propertize "✔" 'face '(:weight bold)))
	  "]"))

(defun m3xan1k/glue-branch-diff (branch-name plus-minus-formatted)
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
	 (plus-minus-formatted (m3xan1k/format-git-diff plus-minus)))
    (setq ad-return-value
	  (m3xan1k/glue-branch-diff ad-return-value plus-minus-formatted))))

;; hide minor modes
(use-package minions
  :config
  (minions-mode t))

;; minimal modeline
(use-package mood-line
  :config
  (mood-line-mode))

;; tabs
(require 'awesome-tab)
(awesome-tab-mode t)

(setq awesome-tab-height 120)
(setq awesome-tab-cycle-scope 'tabs)

(provide 'my-ui)
