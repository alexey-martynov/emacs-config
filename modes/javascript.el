(defun avm-js-mode-hook ()
  (define-key js-mode-map (kbd "<RET>") 'newline-and-indent)
  (define-key js-mode-map (kbd "C-c o") 'ff-find-other-file)
  (setq js-indent-level 2)
  (setq show-trailing-whitespace t)
  (electric-indent-mode t)
  (delete-trailing-whitespace-mode 'clean)
  (fic-mode)
  (add-to-list 'ff-other-file-alist
               '("\\.js$" (".html")))
  )

(add-hook 'js-mode-hook #'avm-js-mode-hook)
