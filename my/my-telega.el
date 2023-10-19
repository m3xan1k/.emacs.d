;; telega stuff

;; telega
(use-package telega
  :ensure t
  :commands
  (telega)
  :general
  (my/leader
   "t" '(:keymap telega-prefix-map :which-key "telega"))
  :config
  (setq telega-use-docker t)
  (setq telega-enable-storage-optimizer t)
  (setq telega-chat-input-markups '("markdown2" nil "org")))

(provide 'my-telega)
