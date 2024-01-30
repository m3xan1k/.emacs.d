;; -------- Speed up load time -------

;; I don't use emacs-server, so startup times are very important to me.

;; Garbage collection is triggered very often during start up, and it
;; slows the whole thing down. It is safe to increase threshold
;; temporarily to prevent aggressive GC, and then re-enable it at the
;; end.

(setq gc-cons-threshold 402653184
      gc-cons-percentage 0.6)


;; There are special ways to handle files (via SSH or in archives),
;; but this is not necessary during startup, and it also slows down
;; the load significantly, as emacs is going through lots of files.
(defvar saved--file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

;; Restore defaults after initialization has completed
(add-hook 'after-init-hook #'(lambda ()
                               (setq gc-cons-threshold 16777216
                                     gc-cons-percentage 0.1)
                               (setq file-name-handler-alist saved--file-name-handler-alist)))
