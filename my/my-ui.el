;; additional theme
(use-package almost-mono-themes)
(load-theme 'almost-mono-cream t)

;; font @
(set-face-attribute 'default nil :font "Source Code Pro" :height 175)
(set-face-attribute 'fringe nil
		    :background (face-background 'default)
		    :foreground (face-foreground 'default))
(set-face-attribute 'line-number nil
                    :background (face-background 'default nil t))
(setq-default line-spacing 1)

;; typography
(set-char-table-range char-width-table '(?— . ?—) 2)

;; tabs
;; (defun m3xan1k-filter-buffers-by-project ()
;;   "Return only buffers that match with the current project."
;;   (let ((root-dir (vc-root-dir)))
;;     (when root-dir
;;       (cl-remove-if-not
;;        (lambda (buf)
;;          (with-current-buffer buf
;;            (and (buffer-file-name)
;;                 (string-prefix-p (expand-file-name root-dir)
;;                                   (expand-file-name buffer-file-name)))))
;;        (buffer-list))))))

(require 's)
(defun my/tab-line-buffer-group (buffer)
  "Use the project.el name for the buffer group"
  (with-current-buffer buffer
    (s-chop-suffix "/" (car (project-roots (project-current))))))

(defun my/buffer-sort (a b) (string< (buffer-name a) (buffer-name b)))

(setq tab-line-tabs-buffer-group-sort-function #'my/buffer-sort)
(setq tab-line-tabs-buffer-group-function #'my/tab-line-buffer-group)
(setq tab-line-tabs-function #'tab-line-tabs-buffer-groups)

(use-package powerline)
(defvar my/tab-height 22)
(defvar my/tab-left (powerline-wave-right 'tab-line nil my/tab-height))
(defvar my/tab-right (powerline-wave-left nil 'tab-line my/tab-height))

(defun my/tab-line-tab-name-buffer (buffer &optional _buffers)
  (powerline-render (list my/tab-left
			  (format " %s  " (buffer-name buffer))
			  my/tab-right)))
(setq tab-line-tab-name-function #'my/tab-line-tab-name-buffer)
(setq tab-line-new-button-show nil)
(setq tab-line-close-button-show nil)

(global-tab-line-mode t)

(set-face-attribute 'tab-line nil ;; background behind tabs
		    :background "gray40"
		    :foreground "gray60" :distant-foreground "gray50"
		    :family "Source Code Pro" :height 1.0 :box nil)
(set-face-attribute 'tab-line-tab nil ;; active tab in another window
		    :inherit 'tab-line
		    :foreground "gray70" :background "gray90" :box nil)
(set-face-attribute 'tab-line-tab-current nil ;; active tab in current window
		    :background "#b34cb3" :foreground "white" :box nil)
(set-face-attribute 'tab-line-tab-inactive nil ;; inactive tab
		    :background "gray80" :foreground "black" :box nil)
(set-face-attribute 'tab-line-highlight nil ;; mouseover
		    :background "white" :foreground 'unspecified)

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

(provide 'my-ui)
