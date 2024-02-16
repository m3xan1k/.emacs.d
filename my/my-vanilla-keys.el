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

;; buffers
(defvar-keymap m3xan1k-buffer-prefix
  :doc "buffer"
  "n" #'awesome-tab-forward
  "p" #'awesome-tab-backward
  "d" #'kill-this-buffer
  "D" #'kill-buffer
  "b" #'consult-buffer
  "r" #'reload-buffer
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
  ";" #'evil-window-split
  "'" #'evil-window-vsplit
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
(global-set-key (kbd "C-=") #'er/expand-region)

;; smart comment
(global-unset-key (kbd "M-;"))
(global-set-key (kbd "M-;") 'm3xan1k-comment)

;; scroll vim-like
(global-set-key (kbd "M-]") 'm3xan1k-scroll-10-lines-down)
(global-set-key (kbd "M-[") 'm3xan1k-scroll-10-lines-up)

;; isearch results selection
(global-unset-key (kbd "C-s"))
(global-set-key (kbd "C-s") 'm3xan1k-consult-line-from-isearch)

;; jump backward/forward
(global-set-key (kbd "C-<") 'jumplist-previous)
(global-set-key (kbd "C->") 'jumplist-next)

;; smart copy
(global-unset-key (kbd "M-w"))
(global-set-key (kbd "M-w") 'm3xan1k-copy)

;; surround
(define-key global-map (kbd "M-'") surround-keymap)

;; New line tweaks
(global-set-key (kbd "C-<return>") 'm3xan1k-new-line-down)
(global-set-key (kbd "C-S-<return>") 'm3xan1k-new-line-up)

;; Tabs shortcuts
(global-set-key (kbd "C-<tab>") #'awesome-tab-forward-tab)
(global-set-key (kbd "C-<iso-lefttab>") #'awesome-tab-backward-tab)

;; navigation in Russian layout
(cl-loop
 for from across "йцукенгшщзхъфывапролджэячсмитьбюЙЦУКЕНГШЩЗХЪФЫВАПРОЛДЖ\ЭЯЧСМИТЬБЮ№"
 for to   across "qwertyuiop[]asdfghjkl;'zxcvbnm,.QWERTYUIOP{}ASDFGHJKL:\"ZXCVBNM<>#"
 do
 (eval `(define-key key-translation-map (kbd ,(concat "C-" (string from))) (kbd ,(concat     "C-" (string to)))))
 (eval `(define-key key-translation-map (kbd ,(concat "M-" (string from))) (kbd ,(concat     "M-" (string to))))))

(scroll-on-jump-advice-add forward-paragraph)
(scroll-on-jump-advice-add backward-paragraph)
(scroll-on-jump-advice-add beginning-of-buffer)
(scroll-on-jump-advice-add end-of-buffer)
(scroll-on-jump-with-scroll-advice-add scroll-down-command)
(scroll-on-jump-with-scroll-advice-add scroll-up-command)

(global-set-key (kbd "C-z g h n") (scroll-on-jump-interactive 'git-gutter:next-hunk))
(global-set-key (kbd "C-z g h p") (scroll-on-jump-interactive 'git-gutter:previous-hunk))

(provide 'my-vanilla-keys)
