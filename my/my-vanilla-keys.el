;; jump forward/backward
(use-package jumplist)

(custom-set-variables
 '(jumplist-hook-commands
   '(dired-jump helm-for-files
     isearch-forward end-of-buffer beginning-of-buffer
     find-file))
 '(jumplist-ex-mode t))

;; smart comment
(defun m3xan1k-comment ()
  "Comment or uncomment line or region."
  (interactive)
  (if (use-region-p)
      (comment-or-uncomment-region (region-beginning) (region-end))
    (comment-or-uncomment-region (line-beginning-position) (line-end-position))))

;; smart copy
(defun m3xan1k-copy ()
  (interactive)
  (if (use-region-p)
      (kill-ring-save (region-beginning) (region-end))
    (kill-ring-save (line-beginning-position) (line-end-position))))

;; half page scroll
(defun m3xan1k-scroll-10-lines-down ()
 "Scroll down 10 lines."
 (interactive)
 (next-line 10)
 (recenter))

(defun m3xan1k-scroll-10-lines-up ()
 "Scroll up 10 lines."
 (interactive)
 (previous-line 10)
 (recenter))

;; keymaps
(use-package general
  :config
  ;; escape on jk
  (general-define-key "j"
		(general-key-dispatch 'self-insert-command
		  :timeout 0.25
		  "k" 'god-local-mode))

  ;; unbind defaults
  (general-unbind
    "C-c"     ;; this is main prefix
    "C-x C-r"   ;; find file read only
    "C-x C-z"   ;; suspend frame
    "C-x C-d"   ;; list directory
    )

  ;; main prefix
  (general-create-definer m3xan1k-prefix
    :prefix "C-c")

  ;; annotation
  (m3xan1k-prefix
    "b" '(:wk "buffer")
    "e" '(:wk "error")
    "f" '(:wk "file")
    "g" '(:wk "git")
    "h" '(:wk "help")
    "p" '(:wk "project")
    "s" '(:wk "search")
    "w" '(:wk "window")
    "j" '(:wk "jump"))

  ;; define custom prefixes
  (general-create-definer m3xan1k-buffer-prefix
    :prefix "C-c b")

  (general-create-definer m3xan1k-diagnostics-prefix
    :prefix "C-c e")

  (general-create-definer m3xan1k-git-prefix
    :prefix "C-c g")

  (general-create-definer m3xan1k-help-prefix
    :prefix "C-c h")

  (general-create-definer m3xan1k-project-prefix
    :prefix "C-c p")

  (general-create-definer m3xan1k-file-prefix
    :prefix "C-c f")

  (general-create-definer m3xan1k-search-prefix
    :prefix "C-c s")

  (general-create-definer m3xan1k-window-prefix
    :prefix "C-c w")

  (general-create-definer m3xan1k-jump-prefix
    :prefix "C-c j")

  ;; error diagnostics
  (m3xan1k-diagnostics-prefix
    "n" '(flymake-goto-next-error :wk "goto-next-error")
    "p" '(flymake-goto-prev-error :wk "goto-prev-error"))

  ;; git hunks
  (m3xan1k-git-prefix
    "h n" '(git-gutter:next-hunk :wk "git-gutter:next-hunk")
    "h p" '(git-gutter:previous-hunk :wk "git-gutter:previous-hunk")
    "h s" '(git-gutter:popup-hunk :wk "git-gutter:popup-hunk")
    "h r" '(git-gutter:revert-hunk :wk "git-gutter:revert-hunk"))

  ;; help
  (m3xan1k-help-prefix
    "h" '(help-command :wk "help-command")
    "s" '(eldoc :wk "eldoc"))

  ;; project
  (m3xan1k-project-prefix
    "p" '(projectile-switch-project :wk "projectile-switch-project")
    "b" '(consult-project-buffer :wk "project buffers")
    "f" '(project-find-file :wk "project-find-file")
    "v" '(project-vc-dir :wk "project-vc-dir"))

  ;; widely used
  (m3xan1k-prefix
    "/" '(consult-ripgrep :wk "search in project")
    "SPC" '(execute-extended-command :wk "execute command")
    ";" '(comment-line :wk "comment line")
    "q q" '(save-buffers-kill-emacs :wk "exit"))

  ;; file
  (m3xan1k-file-prefix
    "s" '(save-buffer :wk "save file")
    "S" '(save-buffers :wk "save all files")
    "f" '(find-file :wk "find file"))

  ;; search
  (m3xan1k-search-prefix
    "r" '(vertico-repeat :wk "resume search")
    "f" '(project-find-file :wk "find file in project")
    "/" '(consult-ripgrep :wk "search in project")
    "c" '(m3xan1k-consult-ripgrep-at-point :wk "m3xan1k-consult-ripgrep-at-point"))

  ;; buffer management
  (m3xan1k-buffer-prefix
    "b" '(:ignore t :wk "buffers")
    "n" '(awesome-tab-forward :wk "next tab")
    "p" '(awesome-tab-backward :wk "previous tab")
    "d" '(kill-this-buffer :wk "close current buffer")
    "D" '(kill-buffer :wk "close buffer interactively")
    "b" '(consult-buffer :wk "search buffers")
    "r" '(reload-buffer :wk "reload buffer")
    "l" '(evil-switch-to-window-last-buffer :wk "last buffer"))

  ;; window management
  (m3xan1k-window-prefix
    "d" '(delete-window :wk "close window")
    "D" '(delete-other-windows :wk "close all other windows")
    "o" '(other-window :wk "switch to other window")
    ";" '(evil-window-split :wk "evil-window-spit")
    "'" '(evil-window-vsplit :wk "evil-window-spit")
    "h" '(evil-window-left :wk "evil-window-left")
    "l" '(evil-window-right :wk "evil-window-right")
    "j" '(evil-window-down :wk "evil-window-down")
    "k" '(evil-window-up :wk "evil-window-up")
    "r" '(:ignore t :wk "resize")
    "r l" '(my/enlarge-window-horizontally :wk "enlarge-window-horizontally")
    "r h" '(my/shrink-window-horizontally :wk "shrink-window-horizontally")
    "r j" '(my/enlarge-window :wk "enlarge-window")
    "r k" '(my/shrink-window :wk "shrink-window"))

  ;; select from the inside
  (general-define-key (kbd "C-=") 'er/expand-region)

  ;; smart comment
  (general-unbind "M-;")
  (general-define-key (kbd "M-;") 'm3xan1k-comment)

  ;; scroll vim-like
  (general-define-key (kbd "M-]") 'm3xan1k-scroll-10-lines-down)
  (general-define-key (kbd "M-[") 'm3xan1k-scroll-10-lines-up)

  ;; isearch results selection
  (general-unbind "C-s")
  (general-define-key (kbd "C-s") 'm3xan1k-consult-line-from-isearch)

  ;; jump backward/forward
  (global-set-key (kbd "C-<") 'jumplist-previous)
  (global-set-key (kbd "C->") 'jumplist-next)

  ;; smart copy
  (general-unbind "M-w")
  (general-define-key (kbd "M-w") 'm3xan1k-copy))

;; surround
(define-key global-map (kbd "M-'") surround-keymap)

;; navigation in Russian layout
(cl-loop
 for from across "йцукенгшщзхъфывапролджэячсмитьбюЙЦУКЕНГШЩЗХЪФЫВАПРОЛДЖ\ЭЯЧСМИТЬБЮ№"
 for to   across "qwertyuiop[]asdfghjkl;'zxcvbnm,.QWERTYUIOP{}ASDFGHJKL:\"ZXCVBNM<>#"
 do
 (eval `(define-key key-translation-map (kbd ,(concat "C-" (string from))) (kbd ,(concat     "C-" (string to)))))
 (eval `(define-key key-translation-map (kbd ,(concat "M-" (string from))) (kbd ,(concat     "M-" (string to))))))

(provide 'my-vanilla-keys)
