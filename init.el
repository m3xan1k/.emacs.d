;; $PATH
(defun set-exec-path-from-shell-PATH ()
  (let ((path-from-shell (replace-regexp-in-string
                          "[ \t\n]*$"
                          ""
                          (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq eshell-path-env path-from-shell) ; for eshell users
    (setq exec-path (split-string path-from-shell path-separator))))

(when window-system (set-exec-path-from-shell-PATH))

;; custom file
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; local elpa
;; to create/update local repo use M-x elpamr-create-mirror-for-installed
;; (add-to-list 'load-path "~/.emacs.d/thirdpart/")
;; (require 'elpa-mirror)

;; use-package
(eval-when-compile
  (require 'use-package))

(require 'use-package-ensure)
(setq use-package-always-ensure t)

;; (setq package-archives
;;       '(("myelpa" . "~/.emacs.d/myelpa/")))

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("gnu" . "http://elpa.gnu.org/packages/")
			 ("myelpa" . "~/.emacs.d/myelpa/")))

;; (setq use-package-always-pin "myelpa")

;;;;;;;;;;;;;;
;; defaults ;;
;;;;;;;;;;;;;;

;; contact
(setq user-full-name "Sergey Shevtsov")
(setq user-mail-address "m3xan1k@duck.com")

;; disable bars
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)

;; cursor
(blink-cursor-mode 0)
(setq x-stretch-cursor t)
(setq cursor-in-non-selected-windows nil)

;; some sane defaults
(setq inhibit-splash-screen t ;; no thanks
      use-file-dialog nil ;; don't use system file dialog
      ring-bell-function 'ignore)

;; line highlight
(global-hl-line-mode 1)

;; line-numbers
(global-display-line-numbers-mode 1)
(setq display-line-numbers-type 'relative)
(line-number-mode 1)
(column-number-mode 1)

;; highlight trailing whitespaces
(setq-default show-trailing-whitespace t)

;; scroll
(setq scroll-conservatively 1)
(setq scroll-margin 1)
(setq scroll-preserve-screen-position t)
(setq next-screen-context-lines (- (/ (window-body-height) 2) 5))

;; use clipboard for cut
(setq select-enable-clipboard t)

;; confirm quit
(setq confirm-kill-emacs 'y-or-n-p)

;; supress warnings
(setq warning-minimum-level :error)

;; remember cursor position when open file again
(save-place-mode 1)

;; recent files history
(recentf-mode 1)

;; refresh buffer when file changes on disk
(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers t)

;; border color column
(add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)

;; word delimiters
(modify-syntax-entry ?_ "w")

;; enable word-wrap
(setq-default truncate-lines nil)

;; formats
(setq display-time-24hr-format t)
(display-time-mode t)

;; no backups
(setq auto-save-default nil)
(setq make-backup-files nil)
(setq auto-save-list-file-name nil)
(setq create-lockfiles nil)

;; for my packages
(setq g-trans-default-source-lang "en")
(setq g-trans-default-target-lang "ru")

;; move to trash on delete
(setq-default delete-by-moving-to-trash t)

;; scratch buffer empty
(setq initial-scratch-message nil)

;; delete selected region on typing
(delete-selection-mode 1)

;; always wrap lines
;; (global-visual-line-mode 1)

;; full path in title bar
;; (setq-default frame-title-format "%b (%f)")

(setq kill-ring-max 50)

;;;;;;;;;
;; lsp ;;
;;;;;;;;;

;; diagnostic popup at point
(use-package flymake-diagnostic-at-point
  :after flymake
  :config
  (add-hook 'flymake-mode-hook #'flymake-diagnostic-at-point-mode))

;; company mode for completion
(use-package company
  :commands company-mode
  :config
  (setq company-idle-delay 0.2)
  (setq company-minimum-prefix-length 1)
  :bind (:map company-active-map
          ("C-n" . company-select-next)
          ("C-j" . company-select-next)
          ("C-k" . company-select-previous)
          ("C-p" . company-select-previous))
  :hook
  (prog-mode . company-mode))

;; help inside company interface
(use-package company-quickhelp)
(eval-after-load 'company
  '(define-key company-active-map (kbd "C-h") #'company-quickhelp-manual-begin))

;; lsp client
(use-package eglot
  :defer t)

;; lsp breadcrumbs
(use-package breadcrumb
  :config
  (breadcrumb-mode t))

;; for expanding snippets
(use-package yasnippet
  :hook ((prog-mode) . yas-minor-mode-on))

;; python
(use-package flymake-ruff
  :hook (eglot-managed-mode . flymake-ruff-load))

(use-package pipenv
  :hook (python-mode . pipenv-mode))

;; activate virtual environment from .venv
(add-hook 'python-mode-hook
    (lambda () (progn
                (pyvenv-activate (concat (project-root (project-current)) ".venv/"))
                (eglot-ensure))))

;; golang
(defun dev/go-mode-hook ()
  (setq tab-width 4))

(use-package go-mode
  :commands go-mode
  :hook
  ((go-mode . eglot-ensure)
   (go-mode . dev/go-mode-hook)))

;; gofmt before save
(defun go-install-save-hooks ()
  (add-hook 'before-save-hook #'eglot-format-buffer t t))
(add-hook 'go-mode-hook #'go-install-save-hooks)

;; markup languages
(use-package yaml-mode
  :commands yaml-mode)
(use-package markdown-mode
  :commands markdown-mode)
;; (use-package haml-mode)

;; w3
(use-package emmet-mode
  :commands emmet-mode
  :init
  (setq emmet-indentation 2)
  (setq emmet-move-cursor-between-quotes t)
  :config
  (add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
  (add-hook 'css-mode-hook  'emmet-mode)) ;; enable Emmet's css abbreviation.

(use-package web-mode
  :commands web-mode
  :config
  (setq web-mode-markup-indent-offset 2)
  (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.js?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.css?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.xml?\\'" . web-mode)))

(defun my-web-mode-hook ()
  (set (make-local-variable 'company-backends) '(company-css company-web-html company-yasnippet company-files)))

(add-hook 'web-mode-hook 'emmet-mode)
(add-hook 'web-mode-hook #'my-web-mode-hook)

;; elisp
(add-hook 'elisp-mode-hook (lambda () (eglot-ensure)))

;; rust
(use-package rust-mode
  :commands rust-mode)
(add-hook 'rust-mode-hook 'eglot-ensure)

(add-hook 'rust-mode-hook
          (lambda () (setq indent-tabs-mode nil)))

;; elixir
(use-package elixir-mode
  :commands elixir-mode)
(add-hook 'elixir-mode-hook 'eglot-ensure)
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs '(elixir-mode "~/soft/lexical/bin/start_lexical.sh")))

;; racket
(use-package racket-mode
  :commands racket-mode)
(add-hook 'racket-mode-hook 'eglot-ensure)
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs '(racket-mode . ("/usr/bin/racket" "-l" "racket-langserver"))))

;; standard ml
(use-package sml-mode
  :commands sml-mode)

;;;;;;;;;;;;
;; search ;;
;;;;;;;;;;;;

;; search choices
(use-package vertico
  :init
  (vertico-mode +1)
  (setq enable-recursive-minibuffers t)
  (add-hook 'minibuffer-setup-hook #'vertico-repeat-save))

;; fuzzy completion
(use-package orderless
  :config
  (setq completion-styles '(orderless)
	completion-category-defaults nil
	completion-category-overrides nil))

;; search itself
(use-package consult
  :config
  (consult-customize
   consult-fd :preview-key '(:debounce 0.4 any)
   consult-async-min-input 1))

(add-to-list 'consult-buffer-filter "^\\*" 'append)

;; inline description
(use-package marginalia
  :init
  (marginalia-mode))

;; grep on project(git repo)
(defun m3xan1k-consult-ripgrep-at-point ()
  (interactive)
  (consult-ripgrep (project-root (project-current)) (thing-at-point 'symbol)))

;; grep on buffer
(defun m3xan1k-consult-line-from-isearch ()
  (interactive)
  (if (region-active-p)
      (progn
	(deactivate-mark)
	(consult-line (buffer-substring (region-beginning) (region-end))))
    (consult-line isearch-string)))

;; preview on find-file
;; (setq read-file-name-function #'consult-find-file-with-preview)

;; (defun consult-find-file-with-preview (prompt &optional dir default mustmatch initial pred)
;;   (interactive)
;;   (let ((default-directory (or dir default-directory))
;;         (minibuffer-completing-file-name t))
;;     (consult--read #'read-file-name-internal :state (consult--file-preview)
;;                    :prompt prompt
;;                    :initial initial
;;                    :require-match mustmatch
;;                    :predicate pred)))

;;;;;;;;;;;
;; tools ;;
;;;;;;;;;;;
;; select from the inside
(use-package expand-region)

;; jump forward/backward
(use-package jumplist)

;; multicursor
(use-package multiple-cursors)

;; linear undo and redo
(use-package undo-fu)

;; surrounding
(use-package surround)

;; which key
(use-package which-key
  :defer 5
  :config
  (which-key-mode))

(setq which-key-idle-delay 0.5)
(setq which-key-idle-secondary-delay 0)
(setq which-key-max-display-columns 5)
(setq which-key-add-column-padding 3)

;; git gutter
(use-package git-gutter
  :defer 5
  :config
  (set-face-background 'git-gutter:modified "orange")
  (set-face-foreground 'git-gutter:modified "orange")
  (set-face-background 'git-gutter:added "green")
  (set-face-foreground 'git-gutter:added "green")
  (set-face-background 'git-gutter:deleted "red")
  (set-face-foreground 'git-gutter:deleted "red"))

(global-git-gutter-mode t)

;; parens
(use-package smartparens
  :config
  (require 'smartparens-config))

(add-hook 'prog-mode-hook #'smartparens-mode)

;; start page
(use-package dashboard
  :commands dashboard-open
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

;; for my packages
(use-package esxml
  :defer 5)

;; file management
(use-package neotree
  :commands neotree-toggle
  :config
  (setq neo-theme 'arrows))

(defun neotree-project-dir ()
  "Open NeoTree using the git root."
  (interactive)
  (let ((project-dir (project-root (project-current)))
        (file-name (buffer-file-name)))
    (neotree-toggle)
    (if project-dir
        (if (neo-global--window-exists-p)
            (progn
              (neotree-dir project-dir)
              (neotree-find file-name)))
      (message "Could not find git project root."))))

;; highlight todos in source code
(use-package hl-todo
  :defer 5
  :init
  (global-hl-todo-mode)
  :config
  (setq hl-todo-keyword-faces
	'(("TODO"   . "#FF0000")
          ("FIXME"  . "#FF0000")
          ("DEBUG"  . "#A020F0")
          ("GOTCHA" . "#FF4500")
          ("STUB"   . "#1E90FF"))))

;; scroll
(use-package scroll-on-jump
  :defer 5
  :config
  (setq scroll-on-jump-duration 0.6
	scroll-on-jump-curve-power 2.0
	scroll-on-jump-curve 'linear))

(scroll-on-jump-advice-add forward-paragraph)
(scroll-on-jump-advice-add backward-paragraph)
(scroll-on-jump-advice-add beginning-of-buffer)
(scroll-on-jump-advice-add end-of-buffer)
(scroll-on-jump-with-scroll-advice-add scroll-down-command)
(scroll-on-jump-with-scroll-advice-add scroll-up-command)
(scroll-on-jump-with-scroll-advice-add recenter-top-bottom)

;; http for my packages
(use-package request
  :defer 5)

;; Show vi-like tilde in the fringe on empty lines.
(use-package vi-tilde-fringe
  :defer 5
  :config
  (global-vi-tilde-fringe-mode 1))

;; terminal
(use-package vterm
  :commands vterm)

;; keycast
;; (use-package keycast
;;   :config
;;   (keycast-mode-line-mode))

;; profile
;; (setq esup-depth 0)
;; (use-package esup)

;;;;;;;;
;; ui ;;
;;;;;;;;

;; Theme
(use-package almost-mono-themes)
(load-theme 'almost-mono-white t)

;; font @
(set-face-attribute 'default nil
		    :font "CommitMono"
		    :height 180
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
		    :background "red3")
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


(set-face-attribute 'font-lock-delimiter-face nil
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

;;;;;;;;;;;;;
;; helpers ;;
;;;;;;;;;;;;;

;; for short snippets
(setq insertion-choices-list '("import ipdb; ipdb.set_trace()"
			       "from pprint import pprint; pprint()"))

(defun m3x-insert-smth ()
  (interactive)
  (insert (completing-read "Insert: " insertion-choices-list nil t)))

;; project-root-terminal
(defun m3x-project-vterm ()
  (interactive)
  (let* ((default-directory (project-root (project-current t)))
	 (vterm-buffer-name (project-prefixed-buffer-name "vterm"))
         (vterm-buffer (get-buffer vterm-buffer-name)))
    (if (and vterm-buffer (not current-prefix-arg))
	(pop-to-buffer vterm-buffer (bound-and-true-p display-comint-buffer-action))
      (vterm))))

;; copy to clipboard current file name
(defun m3xan1k-get-file-name ()
  (interactive)
  (let ((filename (if (y-or-n-p "Absolute?")
                      buffer-file-name
                    (replace-regexp-in-string (project-root (project-current)) "" buffer-file-name))))
    (kill-new filename)
    (message "Filename: %s is copied to clipboard." filename)))

;; copy region to another file
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

;; search for char in line(for vanilla emacs keys)
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

;; diff to specific branch(include uncommitted changes)
(defun m3xan1k-diff-to-branch ()
  (interactive)
  (let ((branch (read-string "Diff to branch: ")))
    (vc-root-version-diff (vc-root-dir) branch nil)))

;; django tests
(defun m3xan1k-run-current-django-test-file ()
  (interactive)
  (let* ((filepath (replace-regexp-in-string (project-root (project-current)) "" buffer-file-name))
	 (dotted-filepath (replace-regexp-in-string "/" "." filepath))
	 (modulename (replace-regexp-in-string "\\.py$" "" dotted-filepath))
	 (keepdb (if (y-or-n-p "keepdb?") "--keepdb" "")))
    (eshell-command (format "%s/manage.py test -v 2 %s %s" (project-root (project-current)) keepdb modulename))))

;; open current .md file in firefox as html
(defun m3xan1k-md-preview-in-firefox ()
  (interactive)
  (let ((html (shell-command-to-string (format "pandoc %s" (shell-quote-argument buffer-file-name)))))
    (eshell-command (format "firefox \"data:text/html;base64,%s\"" (base64-encode-string html)))))

;; smart comment
(defun m3xan1k-comment ()
  "Comment or uncomment line or region."
  (interactive)
  (if (use-region-p)
      (comment-or-uncomment-region (region-beginning) (region-end))
    (comment-or-uncomment-region (line-beginning-position) (line-end-position))))

;; smart copy
(defun m3xan1k-copy ()
  (interactive)
  (if (use-region-p)
      (kill-ring-save (region-beginning) (region-end))
    (progn
      (kill-ring-save (line-beginning-position) (line-end-position))
      (kill-append "\n" nil))))

;; custom resize
(defun m3xan1k-enlarge-window-horizontally ()
  (interactive)
  (enlarge-window-horizontally 5))

(defun m3xan1k-enlarge-window ()
  (interactive)
  (enlarge-window 5))

(defun m3xan1k-shrink-window-horizontally ()
  (interactive)
  (shrink-window-horizontally 5))

(defun m3xan1k-shrink-window ()
  (interactive)
  (shrink-window 5))

(defun m3xan1k-new-line-down ()
  "New line without break."
  (interactive)
  (end-of-line)
  (newline))

(defun m3xan1k-new-line-up ()
  "New line on top without break."
  (interactive)
  (beginning-of-line)
  (newline)
  (previous-line))

;; markdown to html in firefox
;; (defun m3xan1k-md-preview ()
;;   (interactive)
;;   (let* ((html (shell-command-to-string (format "pandoc -s -f markdown -t html %s" buffer-file-name)))
;; 	 (utf-8-html (decode-coding-string html 'utf-8))
;; 	 (encoded-html (base64-encode-string (encode-coding-string utf-8-html 'utf-8))))
;;     (eshell-command (concat
;; 		     "firefox \"data:text/html;base64,"
;; 		     encoded-html
;; 		     "\""))))


;;;;;;;;;
;; org ;;
;;;;;;;;;
;; org defaults
(require 'org)

(defun setup-org-defaults ()
  (setq-local word-wrap t)
  (setq-local fill-column 70)
  (auto-fill-mode t)
  (visual-line-mode t))

(add-hook 'org-mode-hook #'setup-org-defaults)

;; bullets for better visibility
(use-package org-bullets
  :hook
  (org-mode . org-bullets-mode))

(setq org-return-follows-link t)
(setq org-hide-emphasis-markers t)

;; for org-capture notes
(setq org-directory "~/Nextcloud/org")

(setq org-capture-templates
      '(("w" "Work" entry (file+datetree "~/Nextcloud/org/capture/work.org")
         "* %?\nEntered on %U\n  %i\n  %a")
        ("t" "Tech" entry (file+headline "~/Nextcloud/org/capture/tech.org" "Tech")
         "* %?\nEntered on %U\n  %i\n  %a")
        ("p" "Personal" entry (file+headline "~/Nextcloud/org/capture/personal.org" "Personal")
         "* %?\nEntered on %U\n  %i\n  %a")
        ("j" "Journal" entry (file+datetree "~/org/journal.org")
         "* %?\nEntered on %U\n  %i\n  %a")))

;; todos path for agenda
(setq org-agenda-files '("~/Nextcloud/org/agenda"))
(setq org-agenda-inhibit-startup t)
(setq org-agenda-skip-deadline-if-done t)
(setq org-agenda-skip-scheduled-if-done t)

;; some defaults
(setq org-adapt-indentation t)
(setq org-todo-keywords
      '((sequence
	 "NEXT(n)"
	 "TODO(t)"
	 "IN PROGRESS(i)"
	 "WAITING(w)"
	 "SOMEDAY(s)"
	 "PROJ(p)"
	 "REPEAT(r)"
	 "|"
	 "DONE(d)"
	 "CANCELLED(c)")))

;; custom template
(with-eval-after-load "org"
  (add-to-list 'org-structure-template-alist
	       '("my"
		 .
		 "#+TITLE:\n#+TAGS: PHONE(o) COMPUTER(c) SHOPPING(s) URGENT(u)\n#+SEQ_TODO: NEXT(n) TODO(t) WAITING(w) SOMEDAY(s) PROJ(p) INPROGRESS(i) REPEAT(r) | DONE(d) CANCELLED(c)\n#+STARTUP: nologrepeat\n")))

;; for timer
(setq org-clock-sound "~/Nextcloud/music/bell.wav")

;; (use-package denote)
;; (setq denote-directory (expand-file-name "~/Nextcloud/denote/"))
;; (setq denote-org-store-link-to-heading t)

;;;;;;;;;;;;
;; keymaps;;
;;;;;;;;;;;;

;; diagnostics
(defvar-keymap m3xan1k-diagnostics-prefix
  "n" #'flymake-goto-next-error
  "p" #'flymake-goto-prev-error
  "s" #'consult-flymake)

;; git hunks
(defvar-keymap m3xan1k-git-prefix
  ;; "h n" #'git-gutter:next-hunk
  ;; "h p" #'git-gutter:previous-hunk
  "s" #'git-gutter:popup-hunk
  "r" #'git-gutter:revert-hunk)

;; file
(defvar-keymap m3xan1k-file-prefix
  "s" #'save-buffer
  "S" #'save-buffers
  "f" #'find-file
  "e" #'neotree-toggle
  "n" #'m3xan1k-get-file-name
  "r" #'m3xan1k-reopen-killed-file)

;; search
(defvar-keymap m3xan1k-search-prefix
  "r" #'vertico-repeat
  "c" #'m3xan1k-consult-ripgrep-at-point)

;; window resize
(defvar-keymap m3xan1k-window-resize-prefix
  "f" #'m3xan1k-enlarge-window-horizontally
  "b" #'m3xan1k-shrink-window-horizontally
  "n" #'m3xan1k-enlarge-window
  "p" #'m3xan1k-shrink-window)

(which-key-add-keymap-based-replacements window-prefix-map
  "r" `("Resize" . ,m3xan1k-window-resize-prefix))

(which-key-add-keymap-based-replacements vc-prefix-map
  "h" `("Hunks" . ,m3xan1k-git-prefix))

(which-key-add-keymap-based-replacements global-map
  "C-x e" `("Error" . ,m3xan1k-diagnostics-prefix)
  "C-x f" `("File" . ,m3xan1k-file-prefix)
  "C-x s" `("Search" . ,m3xan1k-search-prefix))

(global-unset-key (kbd "C-t"))
(keymap-set hl-todo-mode-map "C-t p" #'hl-todo-previous)
(keymap-set hl-todo-mode-map "C-t n" #'hl-todo-next)
(keymap-set hl-todo-mode-map "C-t o" #'hl-todo-occur)
(keymap-set hl-todo-mode-map "C-t i" #'hl-todo-insert)

;; select from the inside
(global-set-key (kbd "M-=") #'er/expand-region)
(global-set-key (kbd "M--") #'er/contract-region)

;; smart comment
(global-unset-key (kbd "M-;"))
(global-set-key (kbd "M-;") #'m3xan1k-comment)

;; jump backward/forward
(global-unset-key (kbd "C-M-]"))
(global-unset-key (kbd "C-M-["))

(global-set-key (kbd "C-M-[") #'jumplist-previous)
(global-set-key (kbd "C-M-]") #'jumplist-next)

;; smart copy
(global-unset-key (kbd "M-w"))
(global-set-key (kbd "M-w") #'m3xan1k-copy)

;; surround
(define-key global-map (kbd "M-'") surround-keymap)

;; navigation in Russian layout
(cl-loop
 for from across "йцукенгшщзхъфывапролджэячсмитьбюЙЦУКЕНГШЩЗХЪФЫВАПРОЛДЖ\ЭЯЧСМИТЬБЮ№"
 for to   across "qwertyuiop[]asdfghjkl;'zxcvbnm,.QWERTYUIOP{}ASDFGHJKL:\"ZXCVBNM<>#"
 do
 (eval `(define-key key-translation-map (kbd ,(concat "C-" (string from))) (kbd ,(concat     "C-" (string to)))))
 (eval `(define-key key-translation-map (kbd ,(concat "M-" (string from))) (kbd ,(concat     "M-" (string to))))))

;;;; attempt to replicate sublime

;; fuzzy find file in project(git repo)
(global-set-key (kbd "M-p") #'project-find-file)

;; grep in buffer
(global-unset-key (kbd "C-s"))
(global-set-key (kbd "C-s") #'m3xan1k-consult-line-from-isearch)

;; New line tweaks
(global-set-key (kbd "C-<return>") #'m3xan1k-new-line-down)
(global-set-key (kbd "C-S-<return>") #'m3xan1k-new-line-up)

;; jump to char on current line
(define-key global-map (kbd "M-o") #'m3xan1k-jump-to-char)

;; multiple cursors
(define-key mc/keymap (kbd "<return>") nil)
(global-set-key (kbd "M-]") #'mc/mark-next-like-this)
(global-set-key (kbd "M-[") #'mc/unmark-next-like-this)
(global-unset-key (kbd "M-<down-mouse-1>"))
(global-set-key (kbd "M-<mouse-1>") #'mc/add-cursor-on-click)

;; undo-redo
(global-unset-key (kbd "C-_"))
(global-set-key (kbd "C-_") #'undo-fu-only-undo)
(global-set-key (kbd "C-+") #'undo-fu-only-redo)

;; copy/cut/paste
;; (global-unset-key (kbd "C-w"))
;; (global-set-key (kbd "C-w") #'simpleclip-cut)

;; (global-unset-key (kbd "C-y"))
;; (global-set-key (kbd "C-y") #'simpleclip-paste)

;; git diff current changes to some branch
(define-key vc-prefix-map (kbd "B") #'m3xan1k-diff-to-branch)

;; PROJECT
(advice-add #'project-switch-to-buffer :override #'consult-project-buffer)
(advice-add #'project-find-regexp :override #'consult-ripgrep)
;; (advice-add #'project-find-file :override #'consult-find)
(keymap-set project-prefix-map (kbd "t") #'m3x-project-vterm)

;; same for bookmarks
;; (advice-add #'bookmark-jump :override #'consult-bookmark)
(global-unset-key (kbd "C-x r b"))
(keymap-set ctl-x-r-map (kbd "b") #'consult-bookmark)

;; map to C-x
(global-unset-key (kbd "C-x f"))
(global-unset-key (kbd "C-x s"))
(global-unset-key (kbd "C-x e"))
(keymap-set global-map "C-x f" m3xan1k-file-prefix)
(keymap-set global-map "C-x s" m3xan1k-search-prefix)
(keymap-set global-map "C-x e" m3xan1k-diagnostics-prefix)

;; VC
(global-unset-key (kbd "C-x v h"))
(keymap-set vc-prefix-map "h" m3xan1k-git-prefix)
(global-set-key (kbd "C-x v h n") (scroll-on-jump-interactive 'git-gutter:next-hunk))
(global-set-key (kbd "C-x v h p") (scroll-on-jump-interactive 'git-gutter:previous-hunk))

;; window
(keymap-set window-prefix-map (kbd "r") m3xan1k-window-resize-prefix)
(keymap-set window-prefix-map (kbd ";") #'split-window-right)
(keymap-set window-prefix-map (kbd "'") #'split-window-below)

;; eldoc
(global-set-key (kbd "C-c .") 'eldoc)

;; unset annoying
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))
