;; for short snippets
(setq insertion-choices-list '("import ipdb; ipdb.set_trace()"
			       "from pprint import pprint; pprint()"))

(defun m3x-insert-smth ()
  (interactive)
  (insert (completing-read "Insert: " insertion-choices-list nil t)))

;; project-root-terminal
(defun m3x-project-vterm ()
  (interactive)
  (let* ((default-directory (project-root (project-current t)))
	 (vterm-buffer-name (project-prefixed-buffer-name "vterm"))
         (vterm-buffer (get-buffer vterm-buffer-name)))
    (if (and vterm-buffer (not current-prefix-arg))
	(pop-to-buffer vterm-buffer (bound-and-true-p display-comint-buffer-action))
      (vterm))))

;; copy to clipboard current file name
(defun m3xan1k-get-file-name ()
  (interactive)
  (let ((filename (if (y-or-n-p "Absolute?")
                      buffer-file-name
                    (replace-regexp-in-string (projectile-project-root) "" buffer-file-name))))
    (kill-new filename)
    (message "Filename: %s is copied to clipboard." filename)))

;; copy region to another file
(defun m3xan1k-region-to-another-file ()
  "Copies selected region to selected file"
  (interactive)
  (when (use-region-p)
    (kill-ring-save (region-beginning) (region-end))
    (find-file (read-file-name "Pick a file: "))
    (goto-char (point-max))
    (insert "\n")
    (yank)))

;; open url in firefox
(defun m3xan1k-open-in-firefox ()
  (interactive)
  (eshell-command (format "firefox %s" (thing-at-point 'url))))

;; search for char in line(for vanilla emacs keys)
(defun m3xan1k-jump-to-char ()
  (interactive)
  (let ((ch (read-char "Jump to char: ")))

    ;; to search next occurance
    (when (eq ch (char-after))
      (forward-char))

    (search-forward (char-to-string ch) (line-end-position))
    (backward-char)))

;; reopen closed buffer(only if file exists)
(setq m3xan1k-killed-file-list nil)

(defun m3xan1k-add-file-to-killed-file-list ()
  (when buffer-file-name
    (push buffer-file-name m3xan1k-killed-file-list)))

(add-hook 'kill-buffer-hook #'m3xan1k-add-file-to-killed-file-list)

(defun m3xan1k-reopen-killed-file ()
  (interactive)
  (when m3xan1k-killed-file-list
    (find-file (pop m3xan1k-killed-file-list))))

;; diff to specific branch(include uncommitted changes)
(defun m3xan1k-diff-to-branch ()
  (interactive)
  (let ((branch (read-string "Diff to branch: ")))
    (vc-root-version-diff (vc-root-dir) branch nil)))

;; django tests
(defun m3xan1k-run-current-django-test-file ()
  (interactive)
  (let* ((filepath (replace-regexp-in-string (projectile-project-root) "" buffer-file-name))
	 (dotted-filepath (replace-regexp-in-string "/" "." filepath))
	 (modulename (replace-regexp-in-string "\\.py$" "" dotted-filepath))
	 (keepdb (if (y-or-n-p "keepdb?") "--keepdb" "")))
    (eshell-command (format "%s/manage.py test -v 2 %s %s" (projectile-project-root) keepdb modulename))))

(defun m3xan1k-md-preview-in-firefox ()
  (interactive)
  (let ((html (shell-command-to-string (format "pandoc %s" (shell-quote-argument buffer-file-name)))))
    (eshell-command (format "firefox \"data:text/html;base64,%s\"" (base64-encode-string html)))))

;; markdown to html in firefox
;; (defun m3xan1k-md-preview ()
;;   (interactive)
;;   (let* ((html (shell-command-to-string (format "pandoc -s -f markdown -t html %s" buffer-file-name)))
;; 	 (utf-8-html (decode-coding-string html 'utf-8))
;; 	 (encoded-html (base64-encode-string (encode-coding-string utf-8-html 'utf-8))))
;;     (eshell-command (concat
;; 		     "firefox \"data:text/html;base64,"
;; 		     encoded-html
;; 		     "\""))))

(provide 'my-helpers)
