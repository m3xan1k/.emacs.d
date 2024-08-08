;; evil
(setq evil-disable-insert-state-bindings t)
(use-package evil
  :init
  (setq evil-want-C-u-scroll t
	evil-want-Y-yank-to-eol t)
  :config
  (evil-mode 1)
  (evil-set-undo-system 'undo-redo))
  ;; :bind
  ;; ((:map evil-normal-state-map ("/" . 'm3xan1k-consult-line-from-isearch))))

;; same as vim-surround
(use-package evil-surround
  :config
  (global-evil-surround-mode 1))

;; emacs for some modes
(setq m3xan1k-emacs-state-modes '(telega-root-mode
				  telega-chat-mode
				  eww-mode
				  ;; org-mode
				  ;; markdown-mode
				  vterm-mode
				  vc-mode
				  magit-mode))

(defun m3xan1k-apply-emacs-state (modes)
  (while (> (length modes) 0)
    (evil-set-initial-state (pop modes) 'emacs)))

(m3xan1k-apply-emacs-state m3xan1k-emacs-state-modes)

;; custom resize
(defun m3x-enlarge-window-horizontally ()
  (interactive)
  (enlarge-window-horizontally 5))

(defun m3x-enlarge-window ()
  (interactive)
  (enlarge-window 5))

(defun m3x-shrink-window-horizontally ()
  (interactive)
  (shrink-window-horizontally 5))

(defun m3x-shrink-window ()
  (interactive)
  (shrink-window 5))

(defun m3x-set-mark-before-command (orig-fun &rest args)
  (evil-set-jump)
  (apply orig-fun args))

(defun m3x-set-mark-before-commands ()
  (dolist (command '(flymake-goto-next-error
		     flymake-goto-prev-error))
    (advice-add command :around #'m3x-set-mark-before-command)))

(m3x-set-mark-before-commands)

;; keymaps

;; vc
(define-key vc-prefix-map (kbd "B") #'m3xan1k-diff-to-branch)

;; PROJECT
(advice-add #'project-switch-to-buffer :override #'consult-project-buffer)
(advice-add #'project-find-regexp :override #'consult-ripgrep)
;; (advice-add #'project-find-file :override #'consult-find)
(keymap-set project-prefix-map (kbd "t") #'m3x-project-vterm)

(defvar-keymap m3xan1k-buffer-prefix
  :doc "buffer"
  "d" #'kill-this-buffer
  "k" #'kill-buffer
  "s" #'consult-buffer
  "l" #'list-buffers
  "r" #'revert-buffer)

;; diagnostics
(defvar-keymap m3xan1k-diagnostics-prefix
  "n" #'flymake-goto-next-error
  "p" #'flymake-goto-prev-error
  "s" #'consult-flymake)

;; git hunks
(defvar-keymap m3xan1k-git-prefix
  ;; "h n" #'git-gutter:next-hunk
  ;; "h p" #'git-gutter:previous-hunk
  "s" #'git-gutter:popup-hunk
  "r" #'git-gutter:revert-hunk)

;; file
(defvar-keymap m3xan1k-file-prefix
  "s" #'save-buffer
  "S" #'save-buffers
  "f" #'find-file
  "e" #'neotree-toggle
  "n" #'m3xan1k-get-file-name)

;; search
(defvar-keymap m3xan1k-search-prefix
  "r" #'vertico-repeat
  "f" #'project-find-file
  "/" #'consult-ripgrep
  "c" #'m3xan1k-consult-ripgrep-at-point)

;; window resize
(defvar-keymap m3xan1k-window-resize-prefix
  "f" #'m3x-enlarge-window-horizontally
  "b" #'m3x-shrink-window-horizontally
  "n" #'m3x-enlarge-window
  "p" #'m3x-shrink-window)

(defvar-keymap m3xan1k-tab-prefix
  "n" #'awesome-tab-forward-tab
  "p" #'awesome-tab-backward-tab
  "t" #'m3xan1k-reopen-killed-file)

(which-key-add-keymap-based-replacements window-prefix-map
  "r" `("Resize" . ,m3xan1k-window-resize-prefix))

(which-key-add-keymap-based-replacements vc-prefix-map
  "h" `("Hunks" . ,m3xan1k-git-prefix))

;; vc
(global-unset-key (kbd "C-x v h"))
(keymap-set vc-prefix-map "h" m3xan1k-git-prefix)
(global-set-key (kbd "C-x v h n") (scroll-on-jump-interactive 'git-gutter:next-hunk))
(global-set-key (kbd "C-x v h p") (scroll-on-jump-interactive 'git-gutter:previous-hunk))

;; window
(keymap-set window-prefix-map (kbd "r") m3xan1k-window-resize-prefix)
(keymap-set window-prefix-map (kbd ";") 'split-window-right)
(keymap-set window-prefix-map (kbd "'") 'split-window-below)

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
    "M-;"
    "C-x C-z"   ;; unbind suspend frame
    "C-x C-d"   ;; unbind list directory
   "<mouse-2>") ;; pasting with mouse wheel click

  ;; leader
  (general-define-key
    :prefix "SPC"
    :keymaps 'normal
    "" '(nil :which-key "my lieutenant general prefix")
    "p" '(:keymap project-prefix-map :wk "project")
    "v" '(:keymap vc-prefix-map :wk "vc")
    "b" '(:keymap m3xan1k-buffer-prefix :wk "buffer")
    "f" '(:keymap m3xan1k-file-prefix :wk "file")
    "s" '(:keymap m3xan1k-search-prefix :wk "search")
    "w" '(:keymap window-prefix-map :wk "window")
    "e" '(:keymap m3xan1k-diagnostics-prefix :wk "errors"))

  ;; local-leader
  (general-create-definer my/local-leader
    :states '(normal visual)
    :keymaps 'override
    :prefix ",")

  ;; lisp evaluation
  (my/local-leader
   :keymaps 'emacs-lisp-mode-map
   "e" '(:ignore t :wk "elisp")
   "e e" '(eval-last-sexp :wk "eval-last-sexp")
   "e r" '(eval-region :wk "eval region")
   "e d" '(eval-defun :wk "eval root form")
   "e b" '(eval-buffer :wk "eval-buffer"))

  ;; common lisp
  (my/local-leader
    :keymaps 'lisp-mode-map
    "c" '(sly :wk "sly")
    "e" '(:ignore t :wk "eval")
    "e e" '(sly-eval-last-expression :wk "sly-eval-last-expression")
    "e d" '(sly-eval-defun :wk "sly-eval-defun")
    "e b" '(sly-eval-buffer :wk "sly-eval-buffer"))

  ;; sql
  (my/local-leader
    :keymaps 'sql-mode-map
    "e" '(lsp-sql-execute-query :wk "lsp-sql-execute-query"))

   ;; smart comment
   (general-define-key
    :states '(normal emacs)
    :keymaps 'override
    "M-;" 'comment-line)

   (general-define-key
    :states '(visual)
    :keymaps 'override
    "M-;" 'comment-dwim))

(evil-global-set-key 'normal (kbd "SPC g h n") (scroll-on-jump-interactive 'git-gutter:next-hunk))
(evil-global-set-key 'normal (kbd "SPC g h p") (scroll-on-jump-interactive 'git-gutter:previous-hunk))

;; neotree
(evil-define-key 'normal neotree-mode-map (kbd "SPC") 'neotree-quick-look)
(evil-define-key 'normal neotree-mode-map (kbd "a") 'neotree-create-node)
(evil-define-key 'normal neotree-mode-map (kbd "m") 'neotree-rename-node)
(evil-define-key 'normal neotree-mode-map (kbd "d") 'neotree-delete-node)
(evil-define-key 'normal neotree-mode-map (kbd "ESC") 'neotree-hide)
(evil-define-key 'normal neotree-mode-map (kbd "RET") 'neotree-enter)
(evil-define-key 'normal neotree-mode-map (kbd "R") 'neotree-refresh)
(evil-define-key 'normal neotree-mode-map (kbd "p") 'neotree-copy-filepath-to-yank-ring)
(evil-define-key 'normal neotree-mode-map (kbd "c") 'neotree-copy-node)
(evil-define-key 'normal neotree-mode-map (kbd "H") 'neotree-hidden-file-toggle)
(evil-define-key 'normal neotree-mode-map (kbd "A") 'neotree-stretch-toggle)

;; grep in buffer
(global-unset-key (kbd "C-s"))
(global-set-key (kbd "C-s") 'm3xan1k-consult-line-from-isearch)

;; navigation in Russian layout
(cl-loop
 for from across "йцукенгшщзхъфывапролджэячсмитьбюЙЦУКЕНГШЩЗХЪФЫВАПРОЛДЖ\ЭЯЧСМИТЬБЮ№"
 for to   across "qwertyuiop[]asdfghjkl;'zxcvbnm,.QWERTYUIOP{}ASDFGHJKL:\"ZXCVBNM<>#"
 do
 (eval `(define-key key-translation-map (kbd ,(concat "C-" (string from))) (kbd ,(concat     "C-" (string to)))))
 (eval `(define-key key-translation-map (kbd ,(concat "M-" (string from))) (kbd ,(concat     "M-" (string to))))))

(provide 'my-keys)
