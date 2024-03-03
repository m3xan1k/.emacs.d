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
  (consult-line isearch-string))

(provide 'my-search)
