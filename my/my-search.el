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
   consult-fd :preview-key '(:debounce 0.4 any)
   consult-async-min-input 1))

(add-to-list 'consult-buffer-filter "^\\*" 'append)

;; inline description
(use-package marginalia
  :init
  (marginalia-mode))

;; grep on project(git repo)
(defun m3xan1k-consult-ripgrep-at-point ()
  (interactive)
  (consult-ripgrep (projectile-project-root) (thing-at-point 'symbol)))

;; grep on buffer
(defun m3xan1k-consult-line-from-isearch ()
  (interactive)
  (if (region-active-p)
      (progn
	(deactivate-mark)
	(consult-line (buffer-substring (region-beginning) (region-end))))
    (consult-line isearch-string)))

;; preview on find-file
(setq read-file-name-function #'consult-find-file-with-preview)

(defun consult-find-file-with-preview (prompt &optional dir default mustmatch initial pred)
  (interactive)
  (let ((default-directory (or dir default-directory))
        (minibuffer-completing-file-name t))
    (consult--read #'read-file-name-internal :state (consult--file-preview)
                   :prompt prompt
                   :initial initial
                   :require-match mustmatch
                   :predicate pred)))

(provide 'my-search)
