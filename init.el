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

;; use-package
(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "https://melpa.org/packages/") t)

(setq use-package-always-ensure t)
(setq use-package-always-pin "melpa-stable")

;;;;;;;;;;;;;;;;;;
;;  defaults    ;;
;;;;;;;;;;;;;;;;;;

;; disable bars
(tool-bar-mode -1)

(setq default-scroll-bar-width 6)
(scroll-bar-mode -1)

(menu-bar-mode -1)

;; cursor
(blink-cursor-mode 0)
(setq x-stretch-cursor t)
(setq cursor-in-non-selected-windows nil)
(set-cursor-color "red")

(setq inhibit-splash-screen t ;; no thanks
      use-file-dialog nil ;; don't use system file dialog
      tab-bar-new-button-show nil ;; don't show new tab button
      tab-bar-close-button-show nil ;; don't show tab close button
      tab-line-close-button-show nil) ;; don't show tab close button

;; line highlight
(global-hl-line-mode 1)

;; font
(set-face-attribute 'default nil :font "Ricty Diminished" :height 200)

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

;; auto close parenthesis
(electric-pair-mode 1)

;; use clipboard for cut
(setq select-enable-clipboard t)

;; dont close emacs
(setq confirm-kill-emacs 'yes-or-no-p)

;; supress warnings
(setq warning-minimum-level :error)

;; no backup files
(setq make-backup-files nil)
(setq auto-save-default nil)

;; keep backup and save files in a dedicated directory
(setq backup-directory-alist
      `((".*" . ,(concat user-emacs-directory "backups")))
      auto-save-file-name-transforms
      `((".*" ,(concat user-emacs-directory "backups") t)))

 ;; no need to create lockfiles
(setq create-lockfiles nil)

;; remember cursor position when open file again
(save-place-mode 1)

;; recent files history
(recentf-mode 1)

;; change buffer when file changes on disk
(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers t)

;; themes
(use-package almost-mono-themes
  :ensure t
  :config
  ;; (load-theme 'almost-mono-gray t)
  (load-theme 'almost-mono-cream t))

;;;;;;;;;;;;;;;;;;;;
;; general        ;;
;;;;;;;;;;;;;;;;;;;;

(use-package general
  :config
  (general-evil-setup)
  ;; integrate general with evil
  (general-def :states '(normal motion emacs) "SPC" nil)
  ;; set up 'SPC' as the global leader key
  (general-create-definer my/leader
    :states '(normal visual emacs)
    :keymaps 'override
    :prefix "SPC") ;; set leader

  (general-define-key
   :prefix "]"
   :states '(normal)
   :keymaps 'override
   "d" '(flymake-goto-next-error :which-key "goto-next-error")
   "c" '(diff-hl-next-hunk :which-key "diff-hl-next-hunk"))

  (general-define-key
   :prefix "["
   :states '(normal)
   :keymaps 'override
   "d" '(flymake-goto-prev-error :which-key "goto-prev-error")
   "c" '(diff-hl-previous-hunk :which-key "diff-hl-previous-hunk"))

  (my/leader
    "h" '(:ignore t :wk "hunk")
    "h p" '(diff-hl-show-hunk :wk "diff-hl-show-hunk")
    "h r" '(diff-hl-revert-hunk :wk "diff-hl-revert-hunk")
    "h h" '(help-command :wh "help-command"))

  ;; jk to go to normal mode
  (general-imap "j"
    (general-key-dispatch 'self-insert-command
      :timeout 0.25
      "k" 'evil-normal-state))

  ;; unbind some annoying default bindings
  (general-unbind
    "C-x C-r"   ;; unbind find file read only
    "C-x C-z"   ;; unbind suspend frame
    "C-x C-d"   ;; unbind list directory
   "<mouse-2>") ;; pasting with mouse wheel click

  (my/leader
    ;; "SPC" '(execute-extended-command :wk "execute command") ;; an alternative to 'M-x'
    "SPC" '(counsel-M-x :wk "execute command") ;; an alternative to 'M-x'
    "TAB" '(:keymap tab-prefix-map :wk "tab")) ;; remap tab bindings

  (my/leader
   "g" '(:ignore t :wk "lsp")
   "g d" '(lsp-find-definition :wk "lsp-find-definition")
   "g r" '(lsp-find-references :wk "lsp-find-references")
   "g h" '(lsp-ui-doc-glance :wk "lsp-ui-doc-glance")
   "g t" '(lsp-ui-doc-toggle :wk "lsp-ui-doc-toggle"))

  ;; file
  (my/leader
    "f" '(:ignore t :wk "file")
    "f s" '(save-buffer :wk "save file")
    "f r" '(ivy-resume :wk "search resume")
    "f w" '(counsel-ag :wk "grep text")
    "f f" '(fzf-projectile :wk "find file in project"))

  ;; buffer
  (my/leader
    "b" '(:ignore t :wk "buffer")
    "b b" '(ivy-switch-buffer :wk "switch buffer") ;; gets overridden by consult
    "b k" '(kill-this-buffer :wk "kill this buffer")
    "b r" '(revert-buffer :wk "reload buffer")
    "b l" '(evil-switch-to-window-last-buffer :wk "last buffer"))

  (my/leader
   :keymaps 'emacs-lisp-mode-map
   "m" '(:ignore t :wk "elisp")
   "m e" '(:ignore t :wk "eval")
   "m e e" 'eval-last-sexp
   "m e b" 'eval-buffer)

  (my/leader
   :keymaps 'clojure-mode-map
   "m" '(:ignore t :wk "elisp")
   "m e" '(:ignore t :wk "eval")
   "m e e" 'cider-eval-last-sexp
   "m c" 'cider-connect-clj
   "m e b" 'cider-eval-buffer))

;;;;;;;;;;;;;;;;;
;;     evil    ;;
;;;;;;;;;;;;;;;;;

(setq evil-want-C-u-scroll t)
(setq evil-want-Y-yank-to-eol t)

(use-package evil
  :config
  (evil-mode 1)
  (evil-set-undo-system 'undo-redo))

(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;      search            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package counsel)

(use-package ivy-file-preview)
(ivy-file-preview-mode 1)

(setq ivy-use-virtual-buffers t)
(setq ivy-display-style 'fancy)

(use-package ivy
  :ensure t
  :diminish (ivy-mode)
  :config
  (ivy-mode 1))

(use-package ag
  :ensure t)

(use-package ripgrep
  :ensure t)

(use-package fzf
  :ensure t
  :config
  (setq fzf/args "-x --color bw --print-query --margin=1,0 --no-hscroll"
        fzf/executable "fzf"
        fzf/git-grep-args "-i --line-number %s"
        ;; command used for `fzf-grep-*` functions
        ;; example usage for ripgrep:
        fzf/grep-command "rg --no-heading -nH"
        ;; fzf/grep-command "grep -nrH"
        ;; If nil, the fzf buffer will appear at the top of the window
        fzf/position-bottom t
    fzf/window-height 15))

(use-package projectile
  :init
  (projectile-mode +1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;      tools             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package doom-modeline
  :init
  (doom-modeline-mode 1))

(define-key evil-visual-state-map (kbd "M-<down>") (concat ":m '>+1" (kbd "RET") "gv=gv"))
(define-key evil-visual-state-map (kbd "M-<up>")   (concat ":m '<-2" (kbd "RET") "gv=gv"))
(define-key evil-normal-state-map (kbd "M-<down>") (concat ":m +1" (kbd "RET") "=="))
(define-key evil-normal-state-map (kbd "M-<up>")   (concat ":m -2" (kbd "RET") "=="))

;; The maximum displayed length of the branch name of version control.
(setq doom-modeline-vcs-max-length 32)

(use-package which-key
  :config (which-key-mode))

(setq which-key-idle-delay 0.5)
(setq which-key-idle-secondary-delay 0)

(use-package flymake-diagnostic-at-point
  :after flymake
  :config
  (add-hook 'flymake-mode-hook #'flymake-diagnostic-at-point-mode))

(use-package diff-hl)
(global-diff-hl-mode)
(diff-hl-flydiff-mode 1)

(use-package blamer
  :defer 20
  :custom
  (blamer-idle-time 0.3)
  (blamer-min-offset 10)
  :custom-face
  (blamer-face ((t :foreground "#999999"
                    :background nil
                    :height 140
                    :italic nil)))
  :config
  (global-blamer-mode 1))

(use-package centaur-tabs
  :demand
  :config
  (centaur-tabs-mode t)
  :bind
  ("C-h" . centaur-tabs-backward)
  ("C-l" . centaur-tabs-forward))

(centaur-tabs-headline-match)
(setq centaur-tabs-style "bar")
(setq centaur-tabs-plain-icons t)
(setq centaur-tabs-set-bar 'left)
(setq centaur-tabs-height 20)
(setq centaur-tabs-set-modified-marker t)
(centaur-tabs-change-fonts "Ricty Diminished" 160)

(use-package smartparens
  :config
  (require 'smartparens-config)
  :hook
  ((clojure-mode emacs-lisp-mode) . smartparens-mode))

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

(use-package desktop+)

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;       lsp stuff        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Company mode
(use-package company
  :config
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 1)
  :bind (:map company-active-map
          ("C-n" . company-select-next)
          ("C-p" . company-select-previous))
  :hook
  ((emacs-lisp-mode clojure-mode) . company-mode))


;; Lsp mode
(use-package lsp-mode
  :hook
  (lsp-mode . lsp-enable-which-key-integration)
  :config
  (setenv "PATH" (concat
                   "/usr/local/bin" path-separator
                   (getenv "PATH")))
  :hook
  (clojure-mode . lsp))

;; lsp-ui
(use-package lsp-ui
  :commands lsp-ui-mode)

(add-hook 'lsp-ui-doc-frame-hook
          (lambda (frame _w)
            (set-face-attribute 'default frame :font "Ricty Diminished" :height 150)))

;; Python
(use-package lsp-pyright
  :defer t
  :config
  (setq lsp-pyright-disable-language-service nil
	lsp-pyright-diagnostic-mode "openFilesOnly"
	lsp-pyright-typechecking-mode "basic"
	lsp-pyright-auto-search-paths t
	lsp-pyright-use-library-code-for-types t
	lsp-headerline-breadcrumb-mode t
    ;; lsp-pyright-stub-path (concat (getenv "HOME") "/Documents/projects/python-type-stubs")
    )

  :hook ((python-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp-deferred)))))

(setq python-flymake-command '("flake8" "-"))

;; golang
(use-package go-mode)
(add-hook 'go-mode-hook #'lsp-deferred)
;; Set up before-save hooks to format buffer and add/delete imports.
;; Make sure you don't have other gofmt/goimports hooks enabled.
(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t))
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)


;; Clojure
;; M-x lsp-install-server RET clojure-lsp RET
(use-package clojure-mode)
(use-package cider)
