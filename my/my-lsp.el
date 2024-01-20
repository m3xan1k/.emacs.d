;; diagnostic popup at point
(use-package flymake-diagnostic-at-point
  :after flymake
  :config
  (add-hook 'flymake-mode-hook #'flymake-diagnostic-at-point-mode))

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

;; lsp client
(use-package eglot
  :ensure t
  :defer t)

;; lsp breadcrumbs
(use-package breadcrumb
  :ensure t
  :config
  (breadcrumb-mode t))

;; python
(use-package pipenv
  :hook (python-mode . pipenv-mode))

(add-hook 'python-mode-hook
	  (lambda () (eglot-ensure)))

;; golang
(defun dev/go-mode-hook ()
  (setq tab-width 4))

(use-package go-mode
  :hook
  ((go-mode . eglot-ensure)
   (go-mode . dev/go-mode-hook)))

(defun go-install-save-hooks ()
  (add-hook 'before-save-hook #'eglot-format-buffer t t))
(add-hook 'go-mode-hook #'go-install-save-hooks)

;; w3
(use-package emmet-mode
  :ensure t)

(use-package web-mode
  :ensure t)

(add-to-list 'auto-mode-alist '("\\.html$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))

(add-hook 'web-mode-hook 'emmet-mode)

;; ruby
;; (setq lsp-solargraph-server-command '("/home/m3xan1k/.gem/bin/solargraph" "stdio"))
;; (setq lsp-solargraph-use-bundler t)
;; (add-hook 'ruby-mode-hook #'lsp-deferred)

(provide 'my-lsp)
