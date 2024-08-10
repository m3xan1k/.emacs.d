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

;; use-package
(eval-when-compile
  (require 'use-package))

(require 'use-package-ensure)
(setq use-package-always-ensure t)

;; (setq package-archives
;;       '(("myelpa" . "~/.emacs.d/myelpa/")))

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("gnu" . "http://elpa.gnu.org/packages/")
			 ("myelpa" . "~/.emacs.d/myelpa/")))

;; (setq use-package-always-pin "myelpa")

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

;; local elpa
;; to create/update local repo use M-x elpamr-create-mirror-for-installed
(require 'elpa-mirror)

(put 'dired-find-alternate-file 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'downcase-region 'disabled nil)
