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
    (progn
      (kill-ring-save (line-beginning-position) (line-end-position))
      (kill-append "\n" nil))))

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
  "B" #'list-buffers
  "r" #'revert-buffer)

;; diagnostics
(defvar-keymap m3xan1k-diagnostics-prefix
  "n" #'flymake-goto-next-error
  "p" #'flymake-goto-prev-error)

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
  "f" #'m3xan1k-enlarge-window-horizontally
  "b" #'m3xan1k-shrink-window-horizontally
  "n" #'m3xan1k-enlarge-window
  "p" #'m3xan1k-shrink-window)

(defvar-keymap m3xan1k-tab-prefix
  "n" #'awesome-tab-forward-tab
  "p" #'awesome-tab-backward-tab
  "t" #'m3xan1k-reopen-killed-file)

(which-key-add-keymap-based-replacements window-prefix-map
  "r" `("Resize" . ,m3xan1k-window-resize-prefix))

(which-key-add-keymap-based-replacements vc-prefix-map
  "h" `("Hunks" . ,m3xan1k-git-prefix))

(which-key-add-keymap-based-replacements global-map
  "C-x b" `("Buffer" . ,m3xan1k-buffer-prefix)
  "C-x e" `("Error" . ,m3xan1k-diagnostics-prefix)
  "C-x f" `("File" . ,m3xan1k-file-prefix)
  "C-x s" `("Search" . ,m3xan1k-search-prefix))

;; main prefix/leader
;; (global-unset-key (kbd "C-z"))
;; (global-unset-key (kbd "C-x C-z"))
;; (keymap-set global-map "C-z" m3xan1k-prefix)

(global-unset-key (kbd "C-t"))
(keymap-set global-map "C-t" m3xan1k-tab-prefix)

(keymap-set hl-todo-mode-map "C-t p" #'hl-todo-previous)
(keymap-set hl-todo-mode-map "C-t n" #'hl-todo-next)
(keymap-set hl-todo-mode-map "C-t o" #'hl-todo-occur)
(keymap-set hl-todo-mode-map "C-t i" #'hl-todo-insert)

;; select from the inside
;; (global-set-key (kbd "C-=") #'er/expand-region)
;; (global-set-key (kbd "C--") #'er/contract-region)
(global-set-key (kbd "M-=") #'er/expand-region)
(global-set-key (kbd "M--") #'er/contract-region)

;; smart comment
(global-unset-key (kbd "M-;"))
(global-set-key (kbd "M-;") 'm3xan1k-comment)

;; jump backward/forward
(global-unset-key (kbd "C-M-]"))
(global-unset-key (kbd "C-M-["))

(global-set-key (kbd "C-M-[") 'jumplist-previous)
(global-set-key (kbd "C-M-]") 'jumplist-next)

;; (global-set-key (kbd "C-<") 'jumplist-previous)
;; (global-set-key (kbd "C->") 'jumplist-next)
;; (global-set-key (kbd "C-i") 'jumplist-previous)
;; (global-set-key (kbd "C-o") 'jumplist-next)


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

;; copy/cut/paste
;; (global-unset-key (kbd "C-w"))
;; (global-set-key (kbd "C-w") #'simpleclip-cut)

;; (global-unset-key (kbd "C-y"))
;; (global-set-key (kbd "C-y") #'simpleclip-paste)

;; git diff current changes to some branch
(define-key vc-prefix-map (kbd "B") #'m3xan1k-diff-to-branch)

;; consult-project-buffer instead project-buffer
(advice-add #'project-switch-to-buffer :override #'consult-project-buffer)
(advice-add #'project-find-regexp :override #'consult-ripgrep)
;; (advice-add #'project-find-file :override #'consult-find)

;; map to C-x
(global-unset-key (kbd "C-x b"))
(global-unset-key (kbd "C-x f"))
(global-unset-key (kbd "C-x s"))
(global-unset-key (kbd "C-x e"))
(keymap-set global-map "C-x b" m3xan1k-buffer-prefix)
(keymap-set global-map "C-x f" m3xan1k-file-prefix)
(keymap-set global-map "C-x s" m3xan1k-search-prefix)
(keymap-set global-map "C-x e" m3xan1k-diagnostics-prefix)

;; VC
(global-unset-key (kbd "C-x v h"))
(keymap-set vc-prefix-map "h" m3xan1k-git-prefix)
(global-set-key (kbd "C-x v h n") (scroll-on-jump-interactive 'git-gutter:next-hunk))
(global-set-key (kbd "C-x v h p") (scroll-on-jump-interactive 'git-gutter:previous-hunk))

;; window
(keymap-set window-prefix-map (kbd "r") m3xan1k-window-resize-prefix)
(keymap-set window-prefix-map (kbd ";") 'split-window-right)
(keymap-set window-prefix-map (kbd "'") 'split-window-below)

;; (define-key project-prefix-map (kbd "b") 'consult-project-buffer)
;; (define-key project-prefix-map (kbd "g") 'consult-ripgrep)

(provide 'my-vanilla-keys)
