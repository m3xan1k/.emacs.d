;; ====
;; INIT
;; ====

;; load all custom helper functions
(add-to-list 'load-path "~/.emacs.d/")
(require 'helpers)

;; load $PATH
(when window-system (m3xan1k-set-exec-path-from-shell-PATH))

;; custom file
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; ==================
;; PACKAGE MANAGEMENT
;; ==================

;; straight for package management
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

(use-package straight
  :custom
  (straight-use-package-by-default t))

;; ========
;; DEFAULTS
;; ========

;; contact
(setq user-full-name "Sergey Shevtsov")
(setq user-mail-address "m3xan1k@duck.com")

;; disable bars
(tool-bar-mode -1)
(scroll-bar-mode -1)
;; (menu-bar-mode -1)

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
(setq scroll-margin 3)
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

;; Linear undo and redo.
;; (use-package undo-fu)
;; (global-set-key (kbd "s-z")   'undo-fu-only-undo)
;; (global-set-key (kbd "s-Z") 'undo-fu-only-redo)

;; ===
;; LSP
;; ===

;; diagnostic popup at point
(use-package flymake-diagnostic-at-point
  :after flymake
  :config
  (add-hook 'flymake-mode-hook #'flymake-diagnostic-at-point-mode))

;; company mode for completion
(use-package company
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

;; python
(use-package pipenv
  :hook (python-mode . pipenv-mode))

;; activate virtual environment from .venv
(add-hook 'python-mode-hook
    (lambda () (progn
                (pyvenv-activate (concat (projectile-project-root) ".venv/"))
                (eglot-ensure))))

;; golang
(defun dev/go-mode-hook ()
  (setq tab-width 4))

(use-package go-mode
  :hook
  ((go-mode . eglot-ensure)
   (go-mode . dev/go-mode-hook)))

;; gofmt before save
(defun go-install-save-hooks ()
  (add-hook 'before-save-hook #'eglot-format-buffer t t))
(add-hook 'go-mode-hook #'go-install-save-hooks)

;; w3
(use-package emmet-mode)
(use-package web-mode)

(add-to-list 'auto-mode-alist '("\\.html$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))

(add-hook 'web-mode-hook 'emmet-mode)

;; elisp
(add-hook 'elisp-mode-hook (lambda () (eglot-ensure)))

;; clojure
(use-package clojure-mode)
(add-hook 'clojure-mode-hook 'eglot-ensure)

(use-package cider
  :hook
  (clojure-mode . cider-mode)
  :bind
  (:map cider-mode-map
   ("C-, s" . cider-connect)
   ("C-, d" . cider-eval-defun-at-point)
   ("C-, e" . cider-eval-last-sexp)
   ("C-, b" . cider-eval-buffer)))

;; common lisp
(setq inferior-lisp-program "sbcl")
(use-package sly
  :hook
  (common-lisp-mode . sly-mode)
  :bind
  (:map lisp-mode-map
   ("C-, s" . sly)
   ("C-, d" . sly-eval-defun)
   ("C-, e" . sly-eval-last-expression)
   ("C-, r" . sly-eval-region)
   ("C-, b" . sly-eval-buffer)))

;; c
(add-hook 'c-mode-hook #'eglot-ensure)

;; ruby
;; (setq lsp-solargraph-server-command '("/home/m3xan1k/.gem/bin/solargraph" "stdio"))
;; (setq lsp-solargraph-use-bundler t)
;; (add-hook 'ruby-mode-hook #'lsp-deferred)

;; ======
;; SEARCH
;; ======

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
   consult-async-min-input 1))

;; inline description
(use-package marginalia
  :init
  (marginalia-mode))

;; grep on project(git repo)
(defun m3xan1k-consult-ripgrep-at-point ()
  (interactive)
  (consult-ripgrep (projectile-project-root)(thing-at-point 'symbol)))

;; grep on buffer
(defun m3xan1k-consult-line-from-isearch ()
  (interactive)
  (consult-line (thing-at-point 'symbol)))

;; =====
;; TOOLS
;; =====

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
(use-package git-gutter)
(global-git-gutter-mode t)

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

(scroll-on-jump-advice-add forward-paragraph)
(scroll-on-jump-advice-add backward-paragraph)
(scroll-on-jump-advice-add beginning-of-buffer)
(scroll-on-jump-advice-add end-of-buffer)
(scroll-on-jump-with-scroll-advice-add scroll-down-command)
(scroll-on-jump-with-scroll-advice-add scroll-up-command)

(with-eval-after-load 'goto-chg
  (scroll-on-jump-advice-add goto-last-change)
  (scroll-on-jump-advice-add goto-last-change-reverse))

;; select from the inside
(use-package expand-region)

;; jump forward/backward
(use-package jumplist)

;; http for my packages
(use-package request)

;; multicursor
(use-package multiple-cursors)

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

;; keycast
;; (use-package keycast
;;   :config
;;   (keycast-mode-line-mode))

;; profile
;; (use-package esup)

;; ==
;; UI
;; ==

;; Theme
(use-package almost-mono-themes)
(load-theme 'almost-mono-white t)

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
(set-face-attribute 'font-lock-string-face nil
        :foreground "dark green")
(set-face-attribute 'font-lock-type-face nil
        :foreground "dark orange"
        :slant 'normal
        :italic nil)
(set-face-attribute 'font-lock-function-name-face nil
        :foreground "royal blue"
        :slant 'normal
        :weight 'normal)
(set-face-attribute 'font-lock-constant-face nil
        :foreground "brown" :weight 'bold
        :slant 'normal)
(set-face-attribute 'font-lock-comment-delimiter-face nil
        :weight 'bold)

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

;; =============
;; ORG AND NOTES
;; =============

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

(use-package denote)
(setq denote-directory (expand-file-name "~/Nextcloud/denote/"))
(setq denote-org-store-link-to-heading t)

;; =======
;; KEYMAPS
;; =======

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
    (kill-ring-save (line-beginning-position) (line-end-position))))

;; half page scroll
(defun m3xan1k-scroll-10-lines-down ()
 "Scroll down 10 lines."
 (interactive)
 (scroll-on-jump (next-line 10))
 (recenter))

(defun m3xan1k-scroll-10-lines-up ()
 "Scroll up 10 lines."
 (interactive)
 (scroll-on-jump (previous-line 10))
 (recenter))

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

;; buffers
(defvar-keymap m3xan1k-buffer-prefix
  :doc "buffer"
  "n" #'awesome-tab-forward
  "p" #'awesome-tab-backward
  "d" #'kill-this-buffer
  "D" #'kill-buffer
  "b" #'consult-buffer
  "r" #'revert-buffer
  "l" #'evil-switch-to-window-last-buffer)

;; diagnostics
(defvar-keymap m3xan1k-diagnostics-prefix
  "n" #'flymake-goto-next-error
  "p" #'flymake-goto-prev-error)

;; git hunks
(defvar-keymap m3xan1k-git-prefix
  ;; "h n" #'git-gutter:next-hunk
  ;; "h p" #'git-gutter:previous-hunk
  "h s" #'git-gutter:popup-hunk
  "h r" #'git-gutter:revert-hunk)

;; help
(defvar-keymap m3xan1k-help-prefix
  "h" #'help-command
  "s" #'eldoc)

;; project
(defvar-keymap m3xan1k-project-prefix
  "p" #'projectile-switch-project
  "b" #'consult-project-buffer
  "f" #'project-find-file
  "v" #'project-vc-dir)

;; widely used
(defvar-keymap m3xan1k-prefix
  "/" #'consult-ripgrep
  "SPC" #'execute-extended-command
  ";" #'comment-line
  "q q" #'save-buffers-kill-emacs)

;; file
(defvar-keymap m3xan1k-file-prefix
  "s" #'save-buffer
  "S" #'save-buffers
  "f" #'find-file
  "e" #'neotree-toggle
  "n" #'m3xan1k-get-file-name)

;; search
(defvar-keymap m3xan1k-search-prefix
  "r" #'vertico-repeat
  "f" #'project-find-file
  "/" #'consult-ripgrep
  "c" #'m3xan1k-consult-ripgrep-at-point)

;; window resize
(defvar-keymap m3xan1k-window-resize-prefix
  "l" #'m3xan1k-enlarge-window-horizontally
  "h" #'m3xan1k-shrink-window-horizontally
  "j" #'m3xan1k-enlarge-window
  "k" #'m3xan1k-shrink-window)

;; window management
(defvar-keymap m3xan1k-window-prefix
  "d" #'delete-window
  "D" #'delete-other-windows
  "o" #'other-window
  ";" #'split-window-right
  "'" #'split-window-below
  "r" m3xan1k-window-resize-prefix)

;; keymap
(defvar-keymap m3xan1k-prefix
  :doc "m3xan1k-prefix-map"
  "b" m3xan1k-buffer-prefix
  "e" m3xan1k-diagnostics-prefix
  "f" m3xan1k-file-prefix
  "g" m3xan1k-git-prefix
  "h" m3xan1k-help-prefix
  "p" m3xan1k-project-prefix
  "s" m3xan1k-search-prefix
  "w" m3xan1k-window-prefix
  "/" #'consult-ripgrep
  "SPC" #'execute-extended-command
  ";" #'comment-line
  "q q" #'save-buffers-kill-emacs)

(which-key-add-keymap-based-replacements m3xan1k-prefix
  "b" `("Buffer" . ,m3xan1k-buffer-prefix)
  "e" `("Error" . ,m3xan1k-diagnostics-prefix)
  "f" `("File" . ,m3xan1k-file-prefix)
  "g" `("Git" . ,m3xan1k-git-prefix)
  "h" `("Help" . ,m3xan1k-help-prefix)
  "p" `("Project" . ,m3xan1k-project-prefix)
  "s" `("Search" . ,m3xan1k-search-prefix)
  "w" `("Window" . ,m3xan1k-window-prefix))

(which-key-add-keymap-based-replacements m3xan1k-window-prefix
  "r" `("Resize" . ,m3xan1k-window-resize-prefix))

;; main prefix/leader
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))
(keymap-set global-map "C-z" m3xan1k-prefix)

;; select from the inside
(global-set-key (kbd "C-=") #'er/expand-region)

;; smart comment
(global-unset-key (kbd "M-;"))
(global-set-key (kbd "M-;") 'm3xan1k-comment)

;; jump backward/forward
(global-set-key (kbd "C-<") 'jumplist-previous)
(global-set-key (kbd "C->") 'jumplist-next)

;; smart copy
(global-unset-key (kbd "M-w"))
(global-set-key (kbd "M-w") 'm3xan1k-copy)

;; surround
(define-key global-map (kbd "M-'") surround-keymap)

;; navigation in Russian layout
(cl-loop
 for from across "йцукенгшщзхъфывапролджэячсмитьбюЙЦУКЕНГШЩЗХЪФЫВАПРОЛДЖ\ЭЯЧСМИТЬБЮ№"
 for to   across "qwertyuiop[]asdfghjkl;'zxcvbnm,.QWERTYUIOP{}ASDFGHJKL:\"ZXCVBNM<>#"
 do
 (eval `(define-key key-translation-map (kbd ,(concat "C-" (string from))) (kbd ,(concat     "C-" (string to)))))
 (eval `(define-key key-translation-map (kbd ,(concat "M-" (string from))) (kbd ,(concat     "M-" (string to))))))

(global-set-key (kbd "C-z g h n") (scroll-on-jump-interactive 'git-gutter:next-hunk))
(global-set-key (kbd "C-z g h p") (scroll-on-jump-interactive 'git-gutter:previous-hunk))

;;;; attempt to replicate sublime

;; fuzzy find file in project(git repo)
(global-set-key (kbd "M-p") #'project-find-file)

;; grep in project(git repo)
(global-set-key (kbd "C-S-f") #'m3xan1k-consult-ripgrep-at-point)

;; grep in buffer
(global-unset-key (kbd "C-s"))
(global-set-key (kbd "C-s") 'm3xan1k-consult-line-from-isearch)

;; New line tweaks
(global-set-key (kbd "C-<return>") 'm3xan1k-new-line-down)
(global-set-key (kbd "C-S-<return>") 'm3xan1k-new-line-up)

;; Tabs shortcuts
(global-set-key (kbd "C-<tab>") #'awesome-tab-forward-tab)
(global-set-key (kbd "C-<iso-lefttab>") #'awesome-tab-backward-tab)

;; reopen closed tab
(global-set-key (kbd "C-S-t") #'m3xan1k-reopen-killed-file)

;; jump to char on current line
(define-key global-map (kbd "M-o") 'm3xan1k-jump-to-char)

;; multiple cursors
(define-key mc/keymap (kbd "<return>") nil)
(global-set-key (kbd "M-]") 'mc/mark-next-like-this)
(global-set-key (kbd "M-[") 'mc/unmark-next-like-this)
(global-unset-key (kbd "M-<down-mouse-1>"))
(global-set-key (kbd "M-<mouse-1>") 'mc/add-cursor-on-click)

;; ======
;; ADDONS
;; ======

;; my packages
(add-to-list 'load-path "~/.emacs.d/my-packages/cbr")
(require 'cbr)

(add-to-list 'load-path "~/.emacs.d/my-packages/g-trans")
(require 'g-trans)

(add-to-list 'load-path "~/.emacs.d/my-packages/el-weather")
(require 'el-weather)

(put 'dired-find-alternate-file 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
