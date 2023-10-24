;; font
(set-face-attribute 'default nil :font "Input" :height 170)

(use-package berrys-theme
  :ensure t
  :config
  (load-theme 'berrys t))

;; tab bar
(use-package centaur-tabs
  :demand
  :config
  (centaur-tabs-mode t)
  (centaur-tabs-group-by-projectile-project)
  (centaur-tabs-change-fonts "Input" 120))

(setq centaur-tabs-cycle-scope 'tabs)
(setq centaur-tabs-set-modified-marker t)

;; projectile for tabs grouping
(use-package projectile)
(projectile-mode t)

;; disable in dired
(add-hook 'dired-mode-hook 'centaur-tabs-local-mode)

;; The maximum displayed length of the branch name of version control.
(setq doom-modeline-vcs-max-length 32)

;; modeline customization
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
  (let ((cut-length (- doom-modeline-vcs-max-length
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

(use-package doom-modeline
  :ensure t
  :init
  (doom-modeline-mode 1))

;; (setq evil-normal-state-tag   (propertize "|NORMAL|" 'face '((:background "green" :foreground "black" :weight bold)))
;;       evil-emacs-state-tag    (propertize "|EMACS|" 'face '((:background "black" :foreground "white" :weight bold)))
;;       evil-insert-state-tag   (propertize "|INSERT|" 'face '((:background "red") :foreground "white" :weight bold))
;;       evil-motion-state-tag   (propertize "|MOTION|" 'face '((:background "blue") :foreground "white" :weight bold))
;;       evil-visual-state-tag   (propertize "|VISUAL|" 'face '((:background "yellow" :foreground "black" :weight bold)))
;;       evil-operator-state-tag (propertize "|OPERATOR|" 'face '((:background "purple" :weight bold))))

;; (setq-default mode-line-format
;; 	      '("%e"
;; 		" "
;; 		(:eval
;; 		 (when (mode-line-window-selected-p)
;; 		 (format "%s" evil-mode-line-tag)))
;; 		" "
;; 		(:eval (format "%s" (buffer-name)))
;; 		" "
;; 		(:eval (format "%s" (symbol-name major-mode)))
;; 		" "
;; 		(format "%s" flymake-mode-line-counters)
;; 		(vc-mode vc-mode)
;; 		" "
;; 		"%l:%c"))

(provide 'my-ui)
