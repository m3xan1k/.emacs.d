;; evil
(use-package evil
  :config
  (evil-mode 1)
  (evil-set-undo-system 'undo-redo))

(setq evil-want-C-u-scroll t)
(setq evil-want-Y-yank-to-eol t)

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

  (general-create-definer my/local-leader
    :states '(normal visual emacs)
    :keymaps 'override
    :prefix ",") ;; set leader

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

  (my/leader
    "p" '(:ignore t :wk "projectile")
    "p p" 'projectile-switch-project)

  ;; jk to go to normal mode
  (general-imap "j"
    (general-key-dispatch 'self-insert-command
      :timeout 0.25
      "k" 'evil-normal-state))

  ;; unbind some annoying default bindings
  (general-unbind
    "M-j"
    "M-k"
    "C-x C-r"   ;; unbind find file read only
    "C-x C-z"   ;; unbind suspend frame
    "C-x C-d"   ;; unbind list directory
   "<mouse-2>") ;; pasting with mouse wheel click

  (my/leader
    ;; "SPC" '(execute-extended-command :wk "execute command") ;; an alternative to 'M-x'
    "SPC" '(execute-extended-command :wk "execute command") ;; an alternative to 'M-x'
    "TAB" '(:keymap tab-prefix-map :wk "tab")) ;; remap tab bindings

  (my/leader
   "g" '(:ignore t :wk "lsp")
   "g d" '(lsp-find-definition :wk "lsp-find-definition")
   "g r" '(lsp-find-references :wk "lsp-find-references")
   "g t" '(lsp-ui-doc-toggle :wk "lsp-ui-doc-toggle"))

  (general-define-key
   :states '(normal)
   :keymaps 'override
   "K" '(lsp-ui-doc-glance :wk "Signature help"))

  ;; file
  (my/leader
    "f" '(:ignore t :wk "file")
    "f s" '(save-buffer :wk "save file")
    "f r" '(vertico-repeat :wk "search resume")
    "f w" '(consult-ripgrep :wk "grep text")
    "f f" '(fzf-projectile :wk "find file in project"))

  ;; buffer
  (my/leader
    "b" '(:ignore t :wk "buffer")
    "b b" '(consult-buffer :wk "switch buffer") ;; gets overridden by consult
    "b k" '(kill-this-buffer :wk "kill this buffer")
    "b r" '(revert-buffer :wk "reload buffer")
    "b l" '(evil-switch-to-window-last-buffer :wk "last buffer"))

  (my/local-leader
   :keymaps 'emacs-lisp-mode-map
   "e" '(:ignore t :wk "elisp")
   "e e" 'eval-last-sexp
   "e b" 'eval-buffer)

  (my/local-leader
   :keymaps 'clojure-mode-map
   "e" '(:ignore t :wk "clojure")
   "e e" 'cider-eval-last-sexp
   "c" 'cider-connect-clj
   "e b" 'cider-eval-buffer))

(defun move-text-internal (arg)
   (cond
    ((and mark-active transient-mark-mode)
     (if (> (point) (mark))
            (exchange-point-and-mark))
     (let ((column (current-column))
              (text (delete-and-extract-region (point) (mark))))
       (forward-line arg)
       (move-to-column column t)
       (set-mark (point))
       (insert text)
       (exchange-point-and-mark)
       (setq deactivate-mark nil)))
    (t
     (beginning-of-line)
     (when (or (> arg 0) (not (bobp)))
       (forward-line)
       (when (or (< arg 0) (not (eobp)))
            (transpose-lines arg))
       (forward-line -1)))))

(defun move-text-down (arg)
   "Move region (transient-mark-mode active) or current line
  arg lines down."
   (interactive "*p")
   (move-text-internal arg))

(defun move-text-up (arg)
   "Move region (transient-mark-mode active) or current line
  arg lines up."
   (interactive "*p")
   (move-text-internal (- arg)))

(global-set-key (kbd "M-k") 'move-text-up)
(global-set-key (kbd "M-j") 'move-text-down)

(provide 'my-keys)
