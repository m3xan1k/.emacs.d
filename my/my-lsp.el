;; diagnostic
(use-package flymake-diagnostic-at-point
  :after flymake
  :config
  (add-hook 'flymake-mode-hook #'flymake-diagnostic-at-point-mode))

;; Company mode
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

(use-package company-quickhelp)

(eval-after-load 'company
  '(define-key company-active-map (kbd "C-h") #'company-quickhelp-manual-begin))

;; Lsp mode
(use-package lsp-mode
  :hook
  (lsp-mode . lsp-enable-which-key-integration)
  :config
  (setenv "PATH" (concat
                   "/usr/local/bin" path-separator
                   (getenv "PATH")))
  (setq lsp-ui-doc-enabled nil
	lsp-ui-doc-show-with-cursor nil
	lsp-ui-doc-show-with-mouse nil
	lsp-lens-enable nil
	lsp-ui-sideline-enable nil
	lsp-ui-sideline-enable nil
	lsp-modeline-code-actions-enable nil
	lsp-eldoc-enable-hover nil
	lsp-signature-auto-activate nil))

;; lsp-ui
(use-package lsp-ui
  :commands lsp-ui-mode)

;; (setq lsp-ui-doc-enabled nil)
;; (setq pgtk-wait-for-event-timeout nil)
;; (setq make-frame-invisible 0.01)

;; (setq lsp-eldoc-enable-hover nil)

(add-hook 'lsp-ui-doc-frame-hook
          (lambda (frame _w)
            (set-face-attribute 'default frame :font "Input" :height 140)))

;; Python
(use-package lsp-pyright
  :defer t
  :config
  (setq lsp-pyright-disable-language-service nil
	lsp-pyright-diagnostic-mode "openFilesOnly"
	lsp-pyright-typechecking-mode "basic"
	lsp-pyright-auto-search-paths t
	lsp-pyright-use-library-code-for-types t
	lsp-headerline-breadcrumb-mode t)
  :hook ((python-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp-deferred)))))

(setq python-flymake-command '("flake8" "-"))

(use-package pipenv
  :hook (python-mode . pipenv-mode)
  :init
  (setq
   pipenv-projectile-after-switch-function
   #'pipenv-projectile-after-switch-extended))

;; golang
(use-package go-mode)
(add-hook 'go-mode-hook #'lsp-deferred)
;; Set up before-save hooks to format buffer and add/delete imports.
;; Make sure you don't have other gofmt/goimports hooks enabled.
(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t))
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)


;; Lisps
;; M-x lsp-install-server RET clojure-lsp RET
(use-package clojure-mode
  :hook
  (clojure-mode . lsp-deferred))

(use-package racket-mode
  :hook
  (racket-mode . lsp-deferred))

(use-package cider)

(provide 'my-lsp)
