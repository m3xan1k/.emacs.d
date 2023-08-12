;; evil
(use-package evil
  :config
  (evil-mode 1)
  (evil-set-undo-system 'undo-redo))

(setq evil-want-C-u-scroll t)
(setq evil-want-Y-yank-to-eol t)

(define-key evil-visual-state-map (kbd "M-<down>") (concat ":m '>+1" (kbd "RET") "gv=gv"))
(define-key evil-visual-state-map (kbd "M-<up>")   (concat ":m '<-2" (kbd "RET") "gv=gv"))
(define-key evil-normal-state-map (kbd "M-<down>") (concat ":m +1" (kbd "RET") "=="))
(define-key evil-normal-state-map (kbd "M-<up>")   (concat ":m -2" (kbd "RET") "=="))

;; general
(use-package general
  :config
  (general-evil-setup)
  ;; integrate general with evil
  (general-def :states '(normal motion emacs) "SPC" nil)
  ;; set up 'SPC' as the global leader key
  (general-create-definer my/leader
    :states '(normal visual emacs)
    :keymaps 'override
    :prefix "SPC") ;; set leader

  (general-define-key
   :prefix "]"
   :states '(normal)
   :keymaps 'override
   "d" '(flymake-goto-next-error :which-key "goto-next-error")
   "c" '(diff-hl-next-hunk :which-key "diff-hl-next-hunk"))

  (general-define-key
   :prefix "["
   :states '(normal)
   :keymaps 'override
   "d" '(flymake-goto-prev-error :which-key "goto-prev-error")
   "c" '(diff-hl-previous-hunk :which-key "diff-hl-previous-hunk"))

  (my/leader
    "h" '(:ignore t :wk "hunk")
    "h p" '(diff-hl-show-hunk :wk "diff-hl-show-hunk")
    "h r" '(diff-hl-revert-hunk :wk "diff-hl-revert-hunk")
    "h h" '(help-command :wh "help-command"))

  ;; jk to go to normal mode
  (general-imap "j"
    (general-key-dispatch 'self-insert-command
      :timeout 0.25
      "k" 'evil-normal-state))

  ;; unbind some annoying default bindings
  (general-unbind
    "C-x C-r"   ;; unbind find file read only
    "C-x C-z"   ;; unbind suspend frame
    "C-x C-d"   ;; unbind list directory
   "<mouse-2>") ;; pasting with mouse wheel click

  (my/leader
    ;; "SPC" '(execute-extended-command :wk "execute command") ;; an alternative to 'M-x'
    "SPC" '(counsel-M-x :wk "execute command") ;; an alternative to 'M-x'
    "TAB" '(:keymap tab-prefix-map :wk "tab")) ;; remap tab bindings

  (my/leader
   "g" '(:ignore t :wk "lsp")
   "g d" '(lsp-find-definition :wk "lsp-find-definition")
   "g r" '(lsp-find-references :wk "lsp-find-references")
   "g h" '(lsp-ui-doc-glance :wk "lsp-ui-doc-glance")
   "g t" '(lsp-ui-doc-toggle :wk "lsp-ui-doc-toggle"))

  ;; file
  (my/leader
    "f" '(:ignore t :wk "file")
    "f s" '(save-buffer :wk "save file")
    "f r" '(ivy-resume :wk "search resume")
    "f w" '(counsel-ag :wk "grep text")
    "f f" '(fzf-projectile :wk "find file in project"))

  ;; buffer
  (my/leader
    "b" '(:ignore t :wk "buffer")
    "b b" '(ivy-switch-buffer :wk "switch buffer") ;; gets overridden by consult
    "b k" '(kill-this-buffer :wk "kill this buffer")
    "b r" '(revert-buffer :wk "reload buffer")
    "b l" '(evil-switch-to-window-last-buffer :wk "last buffer"))

  (my/leader
   :keymaps 'emacs-lisp-mode-map
   "m" '(:ignore t :wk "elisp")
   "m e" '(:ignore t :wk "eval")
   "m e e" 'eval-last-sexp
   "m e b" 'eval-buffer)

  (my/leader
   :keymaps 'clojure-mode-map
   "m" '(:ignore t :wk "elisp")
   "m e" '(:ignore t :wk "eval")
   "m e e" 'cider-eval-last-sexp
   "m c" 'cider-connect-clj
   "m e b" 'cider-eval-buffer))

(provide 'my-keys)
