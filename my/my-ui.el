;; Theme
(use-package almost-mono-themes)
(load-theme 'almost-mono-white t)

;; font @
(set-face-attribute 'default nil
		    :font "Menlo"
		    :height 160
		    :background "#FDF6E3"
		    :foreground "#222222")

;; interface
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
(set-face-attribute 'header-line nil
		    :foreground (face-background 'default)
		    :background (face-foreground 'default))
(set-face-attribute 'mode-line nil
		    :foreground (face-background 'default)
		    :background (face-foreground 'default))
(set-face-attribute 'breadcrumb-project-crumbs-face nil
		    :background (face-background 'default)
		    :foreground (face-foreground 'default))
(set-face-attribute 'breadcrumb-project-leaf-face nil
		    :background (face-background 'default)
		    :foreground (face-foreground 'default))

;; comments
(set-face-attribute 'font-lock-comment-delimiter-face nil
		    :weight 'bold)

;; variables
(set-face-attribute 'font-lock-variable-name-face nil
		    :background (face-background 'default)
		    :foreground (face-foreground 'default))

;; type/class names
(set-face-attribute 'font-lock-type-face nil
		    :foreground (face-foreground 'default)
		    ;; :foreground "dark orange"
		    :underline t
		    :slant 'normal
		    :italic nil)

;; strings
(set-face-attribute 'font-lock-string-face nil
		    :foreground "green4"
		    :background (face-background 'default))

;; keywords
(set-face-attribute 'font-lock-keyword-face nil
		    :foreground "mediumblue")

;; (set-face-attribute 'highlight nil
;; 		    :foreground (face-background 'default)
;; 		    :background (face-foreground 'default))

;; (set-face-attribute 'font-lock-function-name-face nil
;; 		    :foreground "royal blue"
;; 		    :slant 'normal
;; 		    :weight 'normal)
;; (set-face-attribute 'font-lock-constant-face nil
;; 		    :foreground "brown" :weight 'bold
;; 		    :slant 'normal)

(set-cursor-color "red")

(setq-default line-spacing 1)

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
;; (require 'awesome-tab)
;; (awesome-tab-mode t)

;; (setq awesome-tab-height 120)
;; (setq awesome-tab-cycle-scope 'tabs)

(provide 'my-ui)
