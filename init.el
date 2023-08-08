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

;; themes
(use-package almost-mono-themes
  :ensure t
  :config
  ;; (load-theme 'almost-mono-black t)
  ;; (load-theme 'almost-mono-gray t)
  (load-theme 'almost-mono-cream t)
  ;; (load-theme 'almost-mono-white t)
)
