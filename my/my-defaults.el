;; disable bars
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)

;; cursor
(blink-cursor-mode 0)
(setq x-stretch-cursor t)
(setq cursor-in-non-selected-windows nil)
(set-cursor-color "red")

;; some sane defaults
(setq inhibit-splash-screen t ;; no thanks
      use-file-dialog nil ;; don't use system file dialog
      tab-bar-new-button-show nil ;; don't show new tab button
      tab-bar-close-button-show nil ;; don't show tab close button
      tab-line-close-button-show nil) ;; don't show tab close button

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

;; change buffer when file changes on disk
(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers t)

;; border color column
(add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)

(provide 'my-defaults)
