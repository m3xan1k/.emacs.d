;; evil
(use-package evil
  :init
  (setq evil-want-C-u-scroll t
	evil-want-Y-yank-to-eol t)
  :config
  (evil-mode 1)
  (evil-set-undo-system 'undo-redo))

(evil-set-initial-state 'telega-root-mode 'emacs)
(evil-set-initial-state 'telega-chat-mode 'emacs)
(evil-set-initial-state 'org-mode 'emacs)

;; custom resize

(defun my/enlarge-window-horizontally ()
  (interactive)
  (enlarge-window-horizontally 5))

(defun my/enlarge-window ()
  (interactive)
  (enlarge-window 5))

(defun my/shrink-window-horizontally ()
  (interactive)
  (shrink-window-horizontally 5))

(defun my/shrink-window ()
  (interactive)
  (shrink-window 5))

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
    '(normal motion)
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
    :states '(normal visual)
    :keymaps 'override
    :prefix "SPC")

  ;; local-leader
  (general-create-definer my/local-leader
    :states '(normal visual)
    :keymaps 'override
    :prefix ",")

  ;; error diagnostics
  (my/leader
   "e" '(:ignore t :wk "error")
   "e n" '(flymake-goto-next-error :wk "goto-next-error")
   "e p" '(flymake-goto-prev-error :wk "goto-prev-error"))

  (my/leader
   "g" '(:ignore t :wk "git")
   "g h" '(:ignore t :wk "hunk")
   "g h n" '(git-gutter:next-hunk :wk "git-gutter:next-hunk")
   "g h p" '(git-gutter:previous-hunk :wk "git-gutter:previous-hunk")
   "g h s" '(git-gutter:popup-hunk :wk "git-gutter:popup-hunk")
   "g h r" '(git-gutter:revert-hunk :wk "git-gutter:revert-hunk"))

  ;; help
  (my/leader
    "h" '(:ignore t :wk "help")
    "h h" '(help-command :wk "help-command"))

  ;; project
  (my/leader
    "p" '(:ignore t :wk "project")
    "p p" '(projectile-switch-project :wk "projectile-switch-project")
    "p b" '(consult-project-buffer :wk "project buffers")
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
    "w o" '(other-window :wk "switch to other window")
    "w -" '(evil-window-split :wk "evil-window-spit")
    "w /" '(evil-window-vsplit :wk "evil-window-spit")
    "w h" '(evil-window-left :wk "evil-window-left")
    "w l" '(evil-window-right :wk "evil-window-right")
    "w j" '(evil-window-down :wk "evil-window-down")
    "w k" '(evil-window-up :wk "evil-window-up")
    "w r" '(:ignore t :wk "resize")
    "w r l" '(my/enlarge-window-horizontally :wk "enlarge-window-horizontally")
    "w r h" '(my/shrink-window-horizontally :wk "shrink-window-horizontally")
    "w r j" '(my/enlarge-window :wk "enlarge-window")
    "w r k" '(my/shrink-window :wk "shrink-window"))

  ;; lisp evaluation
  (my/local-leader
   :keymaps 'emacs-lisp-mode-map
   "e" '(:ignore t :wk "elisp")
   "e e" '(eval-last-sexp :wk "eval-last-sexp")
   "e r" '(eval-region :wk "eval region")
   "e d" '(eval-defun :wk "eval root form")
   "e b" '(eval-buffer :wk "eval-buffer"))

  ;; cider for clojure
  (my/local-leader
   :keymaps 'clojure-mode-map
   "c" '(cider-jack-in :wk "connect to nrepl")
   "e" '(:ignore t :wk "clojure")
   "e e" '(cider-eval-last-sexp :wk "cider-eval-last-sexp")
   "e r" '(cider-eval-defun-at-point :wk "cider-eval-defun-at-point")
   "e b" '(cider-eval-buffer :wk "cider-eval-buffer"))

  (my/local-leader
    :keymaps 'racket-mode-map
    "e" '(:ignore t :wk "eval/send")
    "e e" '(racket-send-last-sexp :wk "racket-send-last-sexp")
    "e r" '(racket-send-definition :wk "racket-send-definition")
    "e b" '(racket-run :wk "racket run"))

  (my/local-leader
    :keymaps 'lisp-mode-map
    "e" '(:ignore t :wk "eval")
    "e e" '(sly-eval-last-expression :wk "sly-eval-last-expression")
    "e r" '(sly-eval-defun :wk "sly-eval-defun")
    "e b" '(sly-eval-buffer :wk "sly-eval-buffer"))

  ;; sql
  (my/local-leader
    :keymaps 'sql-mode-map
    "e" '(lsp-sql-execute-query :wk "lsp-sql-execute-query"))

  ;; lsp stuff
  (general-define-key
   :states '(normal)
   :keymaps 'override
   "g h" '(lsp-ui-doc-glance :wk "signature help"))

   ;; quit
   (my/leader
     "q" '(:ignore t :wk "quit")
     "q q" '(save-buffers-kill-terminal :wk "quit emacs")
     "q w" '(quit-window :wk "quit window")))

(provide 'my-keys)
