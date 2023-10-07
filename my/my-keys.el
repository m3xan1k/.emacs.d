;; evil
(use-package evil
  :init
  (setq evil-want-C-u-scroll t
	evil-want-Y-yank-to-eol t)
  :config
  (evil-mode 1)
  (evil-set-undo-system 'undo-redo))

;; general
(use-package general
  :config
  (general-evil-setup)

  ;; escape on jk
  (general-imap "j"
		(general-key-dispatch 'self-insert-command
		  :timeout 0.25
		  "k" 'evil-normal-state))

  ;; make SPC noop
  (general-def
    :states
    '(normal motion emacs)
    "SPC"
    nil)

  ;; unbind some annoying default bindings
  (general-unbind
    "M-j"
    "M-k"
    "C-x C-r"   ;; unbind find file read only
    "C-x C-z"   ;; unbind suspend frame
    "C-x C-d"   ;; unbind list directory
   "<mouse-2>") ;; pasting with mouse wheel click

  ;; leader
  (general-create-definer my/leader
    :states '(normal visual emacs)
    :keymaps 'override
    :prefix "SPC")

  ;; local-leader
  (general-create-definer my/local-leader
    :states '(normal visual emacs)
    :keymaps 'override
    :prefix ",")

  ;; error diagnostics
  (my/leader
   "e" '(:ignore t :wk "error")
   "e n" '(flymake-goto-next-error :wk "goto-next-error")
   "e p" '(flymake-goto-prev-error :wk "goto-prev-error"))

  ;; git
  (my/leader
   "g" '(:ignore t :wk "git")
   "g h" '(:ignore t :wk "hunk")
   "g h n" '(diff-hl-next-hunk :wk "diff-hl-next-hunk")
   "g h p" '(diff-hl-previous-hunk :wk "diff-hl-previous-hunk")
   "g h s" '(diff-hl-show-hunk :wk "diff-hl-show-hunk")
   "g h r" '(diff-hl-revert-hunk :wk "diff-hl-revert-hunk"))

  ;; help
  (my/leader
    "h" '(:ignore t :wk "help")
    "h h" '(help-command :wk "help-command"))

  ;; project
  (my/leader
    "p" '(:ignore t :wk "projectile")
    "p p" '(projectile-switch-project :wk "projectile-switch-project")
    "p f" '(project-find-file :wk "project-find-file"))

  ;; widely used
  (my/leader
    "/" '(consult-ripgrep :wk "search in project")
    "SPC" '(execute-extended-command :wk "execute command")
    ";" '(comment-line :wk "comment line"))

  ;; file
  (my/leader
    "f" '(:ignore t :wk "file")
    "f s" '(save-buffer :wk "save file")
    "f S" '(save-buffers :wk "save all files")
    "f f" '(find-file :wk "find file"))

  ;; search
  (my/leader
    "s" '(:ignore t :wk "search")
    "s r" '(vertico-repeat :wk "resume search")
    "s f" '(project-find-file :wk "find file in project")
    "s /" '(consult-ripgrep :wk "search in project"))

  ;; buffer management
  (my/leader
    "b" '(:ignore t :wk "buffers")
    "b n" '(centaur-tabs-forward :wk "next tab")
    "b p" '(centaur-tabs-backward :wk "previous tab")
    "b d" '(kill-this-buffer :wk "close current buffer")
    "b D" '(kill-buffer :wk "close buffer interactively")
    "b b" '(consult-buffer :wk "search buffers")
    "b r" '(reload-buffer :wk "reload buffer")
    "b l" '(evil-switch-to-window-last-buffer :wk "last buffer"))

  ;; window management
  (my/leader
    "w" '(:ignore t :wk "windows")
    "w d" '(delete-window :wk "close window")
    "w D" '(delete-other-windows :wk "close all other windows")
    "w o" '(other-window :wk "switch to other window"))

  ;; lisp evaluation
  (my/local-leader
   :keymaps 'emacs-lisp-mode-map
   "e" '(:ignore t :wk "elisp")
   "e e" '(eval-last-sexp :wk "eval-last-sexp")
   "e r" '(eval-defun :wk "eval root form")
   "e b" '(eval-buffer :wk "eval-buffer"))

  ;; cider for clojure
  (my/local-leader
   :keymaps 'clojure-mode-map
   "c" '(cider-connect-clj :wk "connect to nrepl")
   "e" '(:ignore t :wk "clojure")
   "e e" '(cider-eval-last-sexp :wk "cider-eval-last-sexp")
   "e r" '(cider-eval-defun-at-point :wk "cider-eval-defun-at-point")
   "e b" '(cider-eval-buffer :wk "cider-eval-buffer"))

  ;; sql
  (my/local-leader
    :keymaps 'sql-mode-map
    "e" '(lsp-sql-execute-query :wk "lsp-sql-execute-query"))

  ;; lsp stuff
  (general-define-key
   :states '(normal)
   :keymaps 'override
   "g h" '(lsp-ui-doc-glance :wk "signature help")

   ;; quit
   (my/leader
     "q" '(:ignore t :wk "quit")
     "q Q" '(save-buffers-kill-terminal :wk "quit emacs")
     "q w" '(quit-window :wk "quit window"))))

(provide 'my-keys)
