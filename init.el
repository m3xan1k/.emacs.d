;; custom file
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; use-package
(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "https://melpa.org/packages/") t)

(setq use-package-always-ensure t)
(setq use-package-always-pin "melpa-stable")

;; disable tool-bar
(tool-bar-mode -1)

;; disable scroll-bar
(setq default-scroll-bar-width 6)
(scroll-bar-mode -1)

;; cursor
(blink-cursor-mode 0)
(setq x-stretch-cursor t)
(setq cursor-in-non-selected-windows nil)
(set-cursor-color "red")

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

;; auto close parenthesis
(electric-pair-mode 1)

;; use clipboard for cut
(setq select-enable-clipboard t)

;; remove this keybinding
(define-key global-map (kbd "C-z") 'nil)

;; dont close emacs
(setq confirm-kill-emacs 'yes-or-no-p)

;; supress warnings
(setq warning-minimum-level :error)

;; themes
(use-package almost-mono-themes
  :ensure t
  :config
  ;; (load-theme 'almost-mono-black t)
  ;; (load-theme 'almost-mono-gray t)
  (load-theme 'almost-mono-cream t)
  ;; (load-theme 'almost-mono-white t)
)

;; (use-package tao-theme
;;   :init
;;   (setq tao-theme-use-sepia t)
;;   (setq tao-theme-sepia-depth 5)
;;   (setq tao-theme-sepia-saturation 1)
;;   (setq tao-theme-use-boxes nil)
;;   :config
;;   (load-theme 'tao-yang t))

(use-package evil
  :config
  (evil-mode 1)
  (evil-define-key 'normal lsp-mode-map (kbd "SPC l") lsp-command-map))


(use-package good-scroll
  :config
  (good-scroll-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;      tools             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package which-key
  :ensure t
  :config (which-key-mode))

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
  (dolist (m '(clojure-mode
               clojurec-mode
               clojurescript-mode
               clojurex-mode))
     (add-to-list 'lsp-language-id-configuration `(,m . "clojure"))))

;; lsp-ui
(use-package lsp-ui
  :commands lsp-ui-mode)

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

;; Clojure
;; M-x lsp-install-server RET clojure-lsp RET
(use-package clojure-mode)
(use-package cider)
