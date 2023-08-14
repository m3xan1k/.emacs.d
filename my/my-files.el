;;;;;;;;;;;;;;;;;;
;;  files       ;;
;;;;;;;;;;;;;;;;;;


;; no backup files
(setq make-backup-files nil)
(setq auto-save-default nil)

;; keep backup and save files in a dedicated directory
(setq backup-directory-alist
      `((".*" . ,(concat user-emacs-directory "backups")))
      auto-save-file-name-transforms
      `((".*" ,(concat user-emacs-directory "backups") t)))

 ;; no need to create lockfiles
(setq create-lockfiles nil)

(setq projectile-project-search-path
      '(("~/Documents/projects/" . 1) ("~/Documents/SPELL/" . 1)))

(provide 'my-files)
