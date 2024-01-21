;; ;; search tools
(use-package vertico
  :init
  (vertico-mode +1)
  (setq enable-recursive-minibuffers t)
  (add-hook 'minibuffer-setup-hook #'vertico-repeat-save))

(use-package orderless
  :config
  (setq completion-styles '(orderless)
	completion-category-defaults nil
	completion-category-overrides nil))

(use-package consult
  :config
  (consult-customize
   consult-async-min-input 1))

(use-package marginalia
  :init
  (marginalia-mode))

(defun m3xan1k-consult-ripgrep-at-point ()
  (interactive)
  (consult-ripgrep (projectile-project-root)(thing-at-point 'symbol)))

(provide 'my-search)
