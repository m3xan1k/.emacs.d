;; custom file
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; use-package
(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "https://melpa.org/packages/") t)

(setq use-package-always-ensure t)
(setq use-package-always-pin "melpa-stable")

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
(set-face-attribute 'default nil :font "Ricty Diminished-18")

;; line-numbers
(global-display-line-numbers-mode 1)
(setq display-line-numbers-type 'relative)
(line-number-mode 1)
(column-number-mode 1)

;; kill line with \n if killing from beginning
(setq kill-whole-line t)

;; highlight trailing whitespaces
(setq-default show-trailing-whitespace t)

;; scroll
(setq scroll-conservatively 3)
(setq scroll-margin 3)

(use-package scroll-on-jump
  :config
  (setq scroll-on-jump-duration 0.8)
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
  (scroll-on-jump-with-scroll-advice-add evil-goto-line)
  (scroll-on-jump-with-scroll-advice-add evil-scroll-down)
  (scroll-on-jump-with-scroll-advice-add evil-scroll-up)
  (scroll-on-jump-with-scroll-advice-add evil-scroll-line-to-center)
  (scroll-on-jump-with-scroll-advice-add evil-scroll-line-to-top)
  (scroll-on-jump-with-scroll-advice-add evil-scroll-line-to-bottom))

(with-eval-after-load 'goto-chg
  (scroll-on-jump-advice-add goto-last-change)
  (scroll-on-jump-advice-add goto-last-change-reverse))

;; auto close parenthesis
(electric-pair-mode 1)

;; use clipboard for cut
(setq select-enable-clipboard t)

;; dont close emacs
(setq confirm-kill-emacs 'yes-or-no-p)

;; supress warnings
(setq warning-minimum-level :error)

;; no backup files
(setq make-backup-files nil) ;; keep everything under vc
(setq auto-save-default nil)

;; keep backup and save files in a dedicated directory
(setq backup-directory-alist
      `((".*" . ,(concat user-emacs-directory "backups")))
      auto-save-file-name-transforms
      `((".*" ,(concat user-emacs-directory "backups") t)))

 ;; no need to create lockfiles
(setq create-lockfiles nil)

;; themes
(use-package almost-mono-themes
  :ensure t
  :config
  ;; (load-theme 'almost-mono-gray t)
  (load-theme 'almost-mono-cream t)
)

;;;;;;;;;;;;;;;;;;;;
;; general        ;;
;;;;;;;;;;;;;;;;;;;;

(use-package general
  :config
  (general-evil-setup)
  ;; integrate general with evil
  (general-def :states '(normal motion emacs) "SPC" nil)
  ;; set up 'SPC' as the global leader key
  (general-create-definer patrl/leader-keys
    :states '(normal visual emacs)
    :keymaps 'override
    :prefix "SPC") ;; set leader

  (general-define-key
   :prefix "]"
   :states '(normal)
   :keymaps 'override
   "d" '(flymake-goto-next-error :whick-key "goto-next-error"))

  (general-define-key
   :prefix "["
   :states '(normal)
   :keymaps 'override
   "d" '(flymake-goto-prev-error :whick-key "goto-prev-error"))

  ;; set up ',' as the local leader key
  (general-create-definer patrl/local-leader-keys
    :states '(normal insert visual emacs)
    :keymaps 'override
    :prefix ",");; set local leader

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


  (patrl/leader-keys
    "SPC" '(execute-extended-command :wk "execute command") ;; an alternative to 'M-x'
    "TAB" '(:keymap tab-prefix-map :wk "tab")) ;; remap tab bindings

  (general-create-definer lsp-leader-def
    :states 'normal
    :keymap 'lsp-command-map
    :prefix "SPC l"
    :wk "lsp")

  (lsp-leader-def
   "gd" '(lsp-find-definition :wk "definition")
   "gr" '(lsp-find-references :wk "references")
   "hg" '(lsp-ui-doc-glance :wk "lsp-ui-doc-glance")
   )

  (patrl/leader-keys
    "c" '(:ignore t :wk "code"))

  ;; help
  ;; namespace mostly used by 'helpful'
  (patrl/leader-keys
    "h" '(:ignore t :wk "help"))

  ;; file
  (patrl/leader-keys
    "f" '(:ignore t :wk "file")
    "ff" '(find-file :wk "find file") ;; gets overridden by consult
    "fs" '(save-buffer :wk "save file"))

  ;; buffer
  ;; see 'bufler' and 'popper'
  (patrl/leader-keys
    "b" '(:ignore t :wk "buffer")
    "bb" '(switch-to-buffer :wk "switch buffer") ;; gets overridden by consult
    "bk" '(kill-this-buffer :wk "kill this buffer")
    "br" '(revert-buffer :wk "reload buffer"))

  ;; code
  ;; see 'flymake'
  (patrl/leader-keys
    "c" '(:ignore t :wk "code"))

  ;; open
  (patrl/leader-keys
    "o" '(:ignore t :wk "open")
    "os" '(speedbar t :wk "speedbar")) ;; TODO this needs some love

  ;; search
  ;; see 'consult'
  (patrl/leader-keys
    "s" '(:ignore t :wk "search"))

  ;; templating
  ;; see 'tempel'
  (patrl/leader-keys
    "t" '(:ignore t :wk "template")))

;;;;;;;;;;;;;;;;;
;;     evil    ;;
;;;;;;;;;;;;;;;;;

(setq evil-want-C-u-scroll t)
(setq evil-want-Y-yank-to-eol t)

(use-package evil
  :config
  (evil-mode 1))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;      tools             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package which-key
  :ensure t
  :config (which-key-mode))

(use-package flymake-diagnostic-at-point
  :after flymake
  :config
  (add-hook 'flymake-mode-hook #'flymake-diagnostic-at-point-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;       lsp stuff        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Company mode
(use-package company
  :config
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-Length 1)
  :bind (:map company-active-map
          ("C-n" . company-select-next)
          ("C-p" . company-select-previous)))

;; Lsp mode
(use-package lsp-mode
  :hook
  (lsp-mode . lsp-enable-which-key-integration)
  :config
  (setenv "PATH" (concat
                   "/usr/local/bin" path-separator
                   (getenv "PATH")))
  ;; (dolist (m '(clojure-mode
  ;;              clojurec-mode
  ;;              clojurescript-mode
  ;;              clojurex-mode))
  ;;   (add-to-list 'lsp-language-id-configuration `(,m . "clojure")))
  )

;; lsp-ui
(use-package lsp-ui
  :commands lsp-ui-mode)

;; which key for lsp
(with-eval-after-load 'lsp-mode
  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration))

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

(use-package pyvenv)

;; Clojure
;; M-x lsp-install-server RET clojure-lsp RET
;; (use-package clojure-mode)
;; (use-package cider)
