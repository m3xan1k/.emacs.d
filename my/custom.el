(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(consult-async-input-throttle 0.2)
 '(consult-async-min-input 1)
 '(consult-buffer-filter
   '("\\` " "\\`\\*Completions\\*\\'" "\\`\\*Flymake log\\*\\'"
     "\\`\\*Semantic SymRef\\*\\'" "\\`\\*tramp/.*\\*\\'" "\\`\\*Messages\\*\\'"
     "\\`\\*scratch\\*\\'" "\\`\\*dashboard\\*\\'"
     "\\`\\*straight-process\\*\\'" "\\`\\*lsp-log\\*\\'" "\\`\\*Warnings\\*\\'"
     "\\`\\*Backtrace\\*\\'" "\\`\\*Flymake log\\*\\'" "\\`\\*Pipenv\\*\\'"))
 '(consult-preview-key '(:debounce 0.4 any))
 '(fill-column 80)
 '(git-gutter:update-interval 1)
 '(org-link-frame-setup
   '((vm . vm-visit-folder-other-frame)
     (vm-imap . vm-visit-imap-folder-other-frame) (gnus . org-gnus-no-new-news)
     (file . find-file) (wl . wl-other-frame)))
 '(package-selected-packages 'nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(mode-line ((t (:family "Dejavu Sans Mono" :height 0.8 :background "gray"))))
 '(trailing-whitespace ((t (:background "red1")))))
