;; for short snippets
(setq insertion-choices-list '("import ipdb; ipdb.set_trace()"
			       "from pprint import pprint; pprint()"))

(defun m3x-insert-smth ()
  (interactive)
  (insert (completing-read "Insert: " insertion-choices-list nil t)))

(provide 'my-helpers)
