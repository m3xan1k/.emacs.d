;; search tools
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

(provide 'my-search)
