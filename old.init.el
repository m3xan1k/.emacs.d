;; $PATH
(defun set-exec-path-from-shell-PATH ()
  (let ((path-from-shell (replace-regexp-in-string
                          "[ \t\n]*$"
                          ""
                          (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq eshell-path-env path-from-shell) ; for eshell users
    (setq exec-path (split-string path-from-shell path-separator))))

(when window-system (set-exec-path-from-shell-PATH))

;; custom file
(setq custom-file "~/.emacs.d/my/custom.el")
(load custom-file)

;; straight for package management
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

(use-package straight
  :custom
  (straight-use-package-by-default t))

;; thirdpart
(add-to-list 'load-path "~/.emacs.d/thirdpart")

;; config files
(add-to-list 'load-path "~/.emacs.d/my")

(require 'my-defaults)
(require 'my-lsp)
(require 'my-search)
(require 'my-tools)
(require 'my-ui)
;; (require 'my-ai)
;; (require 'my-telega)
;; (require 'my-org)
;; (require 'my-mail)
(require 'my-helpers)
(require 'my-vanilla-keys)
;; (require 'my-keys)

;; my packages
(add-to-list 'load-path "~/.emacs.d/my-packages/cbr")
(require 'cbr)

(add-to-list 'load-path "~/.emacs.d/my-packages/g-trans")
(require 'g-trans)

(add-to-list 'load-path "~/.emacs.d/my-packages/el-weather")
(require 'el-weather)

(put 'dired-find-alternate-file 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'downcase-region 'disabled nil)
