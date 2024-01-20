;; diagnostic popup at point
(use-package flymake-diagnostic-at-point
  :after flymake
  :config
  (add-hook 'flymake-mode-hook #'flymake-diagnostic-at-point-mode))

(setq python-flymake-command '("flake8" "-"))

;; company mode for completion
(use-package company
  :config
  (setq company-idle-delay 0.2)
  (setq company-minimum-prefix-length 1)
  :bind (:map company-active-map
          ("C-n" . company-select-next)
          ("C-j" . company-select-next)
          ("C-k" . company-select-previous)
          ("C-p" . company-select-previous))
  :hook
  (prog-mode . company-mode))

;; help inside company interface
(use-package company-quickhelp)
(eval-after-load 'company
  '(define-key company-active-map (kbd "C-h") #'company-quickhelp-manual-begin))

(use-package pipenv
  :hook (python-mode . pipenv-mode))

(use-package eglot
  :ensure t
  :defer t)

(add-hook 'python-mode-hook
	  (lambda () (eglot-ensure)))

;; golang
(defun dev/go-mode-hook ()
  (setq tab-width 4))

(use-package go-mode
  :hook
  ((go-mode . eglot-ensure)
   (go-mode . dev/go-mode-hook)))

;; Set up before-save hooks to format buffer and add/delete imports.
;; Make sure you don't have other gofmt/goimports hooks enabled.
(defun go-install-save-hooks ()
  (add-hook 'before-save-hook #'eglot-format-buffer t t))
(add-hook 'go-mode-hook #'go-install-save-hooks)

(use-package breadcrumb
  :ensure t
  :config
  (breadcrumb-mode t))

;; ruby
;; (setq lsp-solargraph-server-command '("/home/m3xan1k/.gem/bin/solargraph" "stdio"))
;; (setq lsp-solargraph-use-bundler t)
;; (add-hook 'ruby-mode-hook #'lsp-deferred)

;; sql
;; (add-hook 'sql-mode-hook #'lsp-deferred)

(provide 'my-lsp)
