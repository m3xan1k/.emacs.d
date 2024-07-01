;; ======================
;; vanilla helper packages
;; ======================
;; select from the inside
(use-package expand-region)

;; jump forward/backward
(use-package jumplist)

;; multicursor
(use-package multiple-cursors)

;; surround
(require 'surround)

;; linear undo and redo
(use-package undo-fu)

;; ================
;; helper functions
;; ================
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

;; (defun m3xan1k-copy ()
;;   (interactive)
;;   (if (use-region-p)
;;       (progn
;; 	(simpleclip-copy (region-beginning) (region-end))
;; 	(keyboard-quit))
;;     (progn
;;       (set-mark (line-beginning-position))
;;       (end-of-line)
;;       (simpleclip-copy (region-beginning) (region-end))
;;       (keyboard-quit))))

;; half page scroll
(defun m3xan1k-scroll-10-lines-down ()
 "Scroll down 10 lines."
 (interactive)
 (scroll-on-jump (next-line 10))
 (recenter))

(defun m3xan1k-scroll-10-lines-up ()
 "Scroll up 10 lines."
 (interactive)
 (scroll-on-jump (previous-line 10))
 (recenter))

;; custom resize
(defun m3xan1k-enlarge-window-horizontally ()
  (interactive)
  (enlarge-window-horizontally 5))

(defun m3xan1k-enlarge-window ()
  (interactive)
  (enlarge-window 5))

(defun m3xan1k-shrink-window-horizontally ()
  (interactive)
  (shrink-window-horizontally 5))

(defun m3xan1k-shrink-window ()
  (interactive)
  (shrink-window 5))

(defun m3xan1k-new-line-down ()
  "New line without break."
  (interactive)
  (end-of-line)
  (newline))

(defun m3xan1k-new-line-up ()
  "New line on top without break."
  (interactive)
  (beginning-of-line)
  (newline)
  (previous-line))

;; =============
;; keymaps
;; =============

;; buffers
(defvar-keymap m3xan1k-buffer-prefix
  :doc "buffer"
  "n" #'awesome-tab-forward
  "p" #'awesome-tab-backward
  "d" #'kill-this-buffer
  "D" #'kill-buffer
  "b" #'consult-buffer
  "r" #'revert-buffer
  "l" #'evil-switch-to-window-last-buffer)

;; diagnostics
(defvar-keymap m3xan1k-diagnostics-prefix
  "n" #'flymake-goto-next-error
  "p" #'flymake-goto-prev-error)

;; git hunks
(defvar-keymap m3xan1k-git-prefix
  ;; "h n" #'git-gutter:next-hunk
  ;; "h p" #'git-gutter:previous-hunk
  "h s" #'git-gutter:popup-hunk
  "h r" #'git-gutter:revert-hunk)

;; help
(defvar-keymap m3xan1k-help-prefix
  "h" #'help-command
  "s" #'eldoc)

;; project
(defvar-keymap m3xan1k-project-prefix
  "p" #'projectile-switch-project
  "b" #'consult-project-buffer
  "f" #'project-find-file
  "v" #'project-vc-dir)

;; widely used
(defvar-keymap m3xan1k-prefix
  "/" #'consult-ripgrep
  "SPC" #'execute-extended-command
  ";" #'comment-line
  "q q" #'save-buffers-kill-emacs)

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
  "l" #'m3xan1k-enlarge-window-horizontally
  "h" #'m3xan1k-shrink-window-horizontally
  "j" #'m3xan1k-enlarge-window
  "k" #'m3xan1k-shrink-window)

;; window management
(defvar-keymap m3xan1k-window-prefix
  "d" #'delete-window
  "D" #'delete-other-windows
  "o" #'other-window
  ";" #'split-window-right
  "'" #'split-window-below
  "r" m3xan1k-window-resize-prefix)

;; keymap
(defvar-keymap m3xan1k-prefix
  :doc "m3xan1k-prefix-map"
  "b" m3xan1k-buffer-prefix
  "e" m3xan1k-diagnostics-prefix
  "f" m3xan1k-file-prefix
  "g" m3xan1k-git-prefix
  "h" m3xan1k-help-prefix
  "p" m3xan1k-project-prefix
  "s" m3xan1k-search-prefix
  "w" m3xan1k-window-prefix
  "/" #'consult-ripgrep
  "SPC" #'execute-extended-command
  ";" #'comment-line
  "q q" #'save-buffers-kill-emacs)

(which-key-add-keymap-based-replacements m3xan1k-prefix
  "b" `("Buffer" . ,m3xan1k-buffer-prefix)
  "e" `("Error" . ,m3xan1k-diagnostics-prefix)
  "f" `("File" . ,m3xan1k-file-prefix)
  "g" `("Git" . ,m3xan1k-git-prefix)
  "h" `("Help" . ,m3xan1k-help-prefix)
  "p" `("Project" . ,m3xan1k-project-prefix)
  "s" `("Search" . ,m3xan1k-search-prefix)
  "w" `("Window" . ,m3xan1k-window-prefix))

(which-key-add-keymap-based-replacements m3xan1k-window-prefix
  "r" `("Resize" . ,m3xan1k-window-resize-prefix))

;; main prefix/leader
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))
(keymap-set global-map "C-z" m3xan1k-prefix)

;; select from the inside
;; (global-set-key (kbd "C-=") #'er/expand-region)
;; (global-set-key (kbd "C--") #'er/contract-region)
(global-set-key (kbd "M-=") #'er/expand-region)
(global-set-key (kbd "M--") #'er/contract-region)

;; smart comment
(global-unset-key (kbd "M-;"))
(global-set-key (kbd "M-;") 'm3xan1k-comment)

;; jump backward/forward
;; (global-set-key (kbd "C-<") 'jumplist-previous)
;; (global-set-key (kbd "C->") 'jumplist-next)
(global-set-key (kbd "C-i") 'jumplist-previous)
(global-set-key (kbd "C-o") 'jumplist-next)

;; smart copy
(global-unset-key (kbd "M-w"))
(global-set-key (kbd "M-w") 'm3xan1k-copy)

;; surround
(define-key global-map (kbd "M-'") surround-keymap)

;; navigation in Russian layout
(cl-loop
 for from across "йцукенгшщзхъфывапролджэячсмитьбюЙЦУКЕНГШЩЗХЪФЫВАПРОЛДЖ\ЭЯЧСМИТЬБЮ№"
 for to   across "qwertyuiop[]asdfghjkl;'zxcvbnm,.QWERTYUIOP{}ASDFGHJKL:\"ZXCVBNM<>#"
 do
 (eval `(define-key key-translation-map (kbd ,(concat "C-" (string from))) (kbd ,(concat     "C-" (string to)))))
 (eval `(define-key key-translation-map (kbd ,(concat "M-" (string from))) (kbd ,(concat     "M-" (string to))))))

(global-set-key (kbd "C-z g h n") (scroll-on-jump-interactive 'git-gutter:next-hunk))
(global-set-key (kbd "C-z g h p") (scroll-on-jump-interactive 'git-gutter:previous-hunk))

;;;; attempt to replicate sublime

;; fuzzy find file in project(git repo)
(global-set-key (kbd "M-p") #'project-find-file)

;; grep in project(git repo)
(global-set-key (kbd "C-S-f") #'m3xan1k-consult-ripgrep-at-point)

;; grep in buffer
(global-unset-key (kbd "C-s"))
(global-set-key (kbd "C-s") 'm3xan1k-consult-line-from-isearch)

;; New line tweaks
(global-set-key (kbd "C-<return>") 'm3xan1k-new-line-down)
(global-set-key (kbd "C-S-<return>") 'm3xan1k-new-line-up)

;; Tabs shortcuts
(global-set-key (kbd "C-<tab>") #'awesome-tab-forward-tab)
(global-set-key (kbd "C-<iso-lefttab>") #'awesome-tab-backward-tab)

;; reopen closed tab
(global-set-key (kbd "C-S-t") #'m3xan1k-reopen-killed-file)

;; jump to char on current line
(define-key global-map (kbd "M-o") 'm3xan1k-jump-to-char)

;; multiple cursors
(define-key mc/keymap (kbd "<return>") nil)
(global-set-key (kbd "M-]") 'mc/mark-next-like-this)
(global-set-key (kbd "M-[") 'mc/unmark-next-like-this)
(global-unset-key (kbd "M-<down-mouse-1>"))
(global-set-key (kbd "M-<mouse-1>") 'mc/add-cursor-on-click)

;; undo-redo
(global-unset-key (kbd "C-_"))
(global-set-key (kbd "C-_") #'undo-fu-only-undo)
(global-set-key (kbd "C-+") #'undo-fu-only-redo)

;; file manager
(global-set-key (kbd "C-S-b") #'neotree-toggle)

;; copy/cut/paste
;; (global-unset-key (kbd "C-w"))
;; (global-set-key (kbd "C-w") #'simpleclip-cut)

;; (global-unset-key (kbd "C-y"))
;; (global-set-key (kbd "C-y") #'simpleclip-paste)

;; git diff current changes to some branch
(define-key vc-prefix-map (kbd "B") #'m3xan1k-diff-to-branch)

(provide 'my-vanilla-keys)
