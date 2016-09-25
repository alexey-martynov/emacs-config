(defun avm-js-mode-hook ()
  (define-key js-mode-map (kbd "<RET>") 'newline-and-indent)
  (define-key js-mode-map (kbd "C-c o") 'ff-find-other-file)
  (make-local-variable 'js-indent-level)
  (setq js-indent-level 2)
  (setq show-trailing-whitespace t)
  (electric-indent-mode t)
  (delete-trailing-whitespace-mode 'clean)
  (fic-mode)
  (add-to-list 'ff-other-file-alist
               '("\\.js$" (".html")))
  )

(when (locate-library "js2-mode")
  (add-hook 'js2-mode-hook #'avm-js-mode-hook)
  (setf (cdr (assoc "\\.jsm?\\'" auto-mode-alist)) 'js2-mode))

(unless (locate-library "js2-mode")
  (add-hook 'js-mode-hook #'avm-js-mode-hook))
