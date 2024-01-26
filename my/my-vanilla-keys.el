(use-package general
  :config
  ;; unbind defaults
  (general-unbind
    "C-x C-r"   ;; find file read only
    "C-x C-z"   ;; suspend frame
    "C-x C-d"   ;; list directory
    )

  ;; define custom prefixes
  (general-create-definer m3xan1k-buffer-prefix
    :prefix "C-, b")

  (general-create-definer m3xan1k-diagnostics-prefix
    :prefix "C-, e")

  (general-create-definer m3xan1k-git-prefix
    :prefix "C-, g")

  (general-create-definer m3xan1k-help-prefix
    :prefix "C-, h")

  (general-create-definer m3xan1k-project-prefix
    :prefix "C-, p")

  (general-create-definer m3xan1k-file-prefix
    :prefix "C-, f")

  (general-create-definer m3xan1k-search-prefix
    :prefix "C-, s")

  (general-create-definer m3xan1k-window-prefix
    :prefix "C-, w")

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
    "h" '(help-command :wk "help-command"))

  ;; project
  (m3xan1k-project-prefix
    "p" '(projectile-switch-project :wk "projectile-switch-project")
    "b" '(consult-project-buffer :wk "project buffers")
    "f" '(project-find-file :wk "project-find-file"))

  ;; widely used
  (general-define-key
   :prefix "C-,"
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

  (general-define-key (kbd "C-=") 'er/expand-region)
  )

(define-key global-map (kbd "M-'") surround-keymap)

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

(global-set-key (kbd "M-<down>") 'm3xan1k-scroll-10-lines-down)
(global-set-key (kbd "M-<up>") 'm3xan1k-scroll-10-lines-up)

(global-unset-key (kbd "M-;"))
(global-set-key (kbd "M-;") 'comment-line)

(provide 'my-vanilla-keys)
