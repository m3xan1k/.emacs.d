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
  :defer t)

;; lsp breadcrumbs
(use-package breadcrumb
  :config
  (breadcrumb-mode t))

;; python
(use-package pipenv
  :hook (python-mode . pipenv-mode))

;; activate virtual environment from .venv
(add-hook 'python-mode-hook
    (lambda () (progn
                (pyvenv-activate (concat (projectile-project-root) ".venv/"))
                (eglot-ensure))))

;; golang
(defun dev/go-mode-hook ()
  (setq tab-width 4))

(use-package go-mode
  :hook
  ((go-mode . eglot-ensure)
   (go-mode . dev/go-mode-hook)))

;; gofmt before save
(defun go-install-save-hooks ()
  (add-hook 'before-save-hook #'eglot-format-buffer t t))
(add-hook 'go-mode-hook #'go-install-save-hooks)

;; markup languages
(use-package yaml-mode)
(use-package haml-mode)
(use-package markdown-mode)

;; w3
(use-package emmet-mode
  :commands emmet-mode
  :init
  (setq emmet-indentation 2)
  (setq emmet-move-cursor-between-quotes t)
  :config
  (add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
  (add-hook 'css-mode-hook  'emmet-mode)) ;; enable Emmet's css abbreviation.

(use-package web-mode
  :config
  (setq web-mode-markup-indent-offset 2)
  (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.js?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.css?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.xml?\\'" . web-mode)))

(defun my-web-mode-hook ()
  (set (make-local-variable 'company-backends) '(company-css company-web-html company-yasnippet company-files)))

(add-hook 'web-mode-hook 'emmet-mode)
(add-hook 'web-mode-hook #'my-web-mode-hook)

;; elisp
(add-hook 'elisp-mode-hook (lambda () (eglot-ensure)))

;; clojure
(use-package clojure-mode)
(add-hook 'clojure-mode-hook 'eglot-ensure)

(use-package cider
  :hook
  (clojure-mode . cider-mode)
  :bind
  (:map cider-mode-map
   ("C-, s" . cider-connect)
   ("C-, d" . cider-eval-defun-at-point)
   ("C-, e" . cider-eval-last-sexp)
   ("C-, b" . cider-eval-buffer)))

;; common lisp
(setq inferior-lisp-program "sbcl")
(use-package sly
  :hook
  (common-lisp-mode . sly-mode)
  :bind
  (:map lisp-mode-map
   ("C-, s" . sly)
   ("C-, d" . sly-eval-defun)
   ("C-, e" . sly-eval-last-expression)
   ("C-, r" . sly-eval-region)
   ("C-, b" . sly-eval-buffer)))

;; c
(add-hook 'c-mode-hook #'eglot-ensure)

;; ruby
(use-package robe)
(add-hook 'ruby-mode-hook 'robe-mode)
(add-hook 'ruby-ts-mode-hook 'robe-mode)
(eval-after-load 'company
  '(push 'company-robe company-backends))

(use-package flymake-ruby)
(add-hook 'ruby-mode-hook 'flymake-ruby-load)

;; (with-eval-after-load 'eglot
;;   (add-to-list 'eglot-server-programs
;;                `(ruby-mode . ("bundle" "exec" "rubocop" "--lsp"))))

;; (add-hook 'ruby-mode #'eglot-ensure)
;; (add-hook 'ruby-ts-mode #'eglot-ensure)

;; (setq lsp-solargraph-server-command '("/home/m3xan1k/.gem/bin/solargraph" "stdio"))
;; (setq lsp-solargraph-use-bundler t)
;; (add-hook 'ruby-mode-hook #'lsp-deferred)

(provide 'my-lsp)
