(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(consult-async-input-throttle 0.2)
 '(consult-async-min-input 1)
 '(consult-preview-key '(:debounce 0.4 any))
 '(eglot-connect-timeout 500)
 '(fill-column 80)
 '(git-gutter:update-interval 1)
 '(jumplist-ex-mode t)
 '(jumplist-hook-commands
   '(dired-jump helm-for-files isearch-forward end-of-buffer beginning-of-buffer find-file))
 '(org-link-frame-setup
   '((vm . vm-visit-folder-other-frame)
     (vm-imap . vm-visit-imap-folder-other-frame)
     (gnus . org-gnus-no-new-news)
     (file . find-file)
     (wl . wl-other-frame)))
 '(package-selected-packages 'nil)
 '(spacemacs-theme-comment-bg nil)
 '(spacemacs-theme-comment-italic t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(awesome-tab-selected-face ((t (:foreground unspecified :distant-foreground unspecified))))
 '(mode-line ((t (:family "Menlo" :height 0.8 :background "gray80" :box nil))))
 '(mode-line-inactive ((t (:family "Menlo" :height 0.8 :background "gray90" :box nil))))
 '(org-block ((t (:background "#fdf6e3" :family "FreeMono" :height 175))))
 '(org-block-begin-line ((t (:background "gray75"))))
 '(org-indent ((t (:background "gray75" :foreground "gray78"))))
 '(trailing-whitespace ((t (:background "red1")))))
