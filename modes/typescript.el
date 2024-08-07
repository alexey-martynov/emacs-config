(defun avm-typescript-mode-hook ()
  (define-key typescript-mode-map (kbd "C-c o") 'ff-find-other-file)
  (define-key typescript-mode-map (kbd "<f7>") 'compile)
  (setf typescript-indent-level 2)

  (setq-local show-trailing-whitespace t)
  (delete-trailing-whitespace-mode 'clean)

  (setf ts-other-file-alist '(("\\.ts$" (".html"))))
  (setf ff-other-file-alist 'ts-other-file-alist)
  )

(when (locate-library "typescript-mode")
  (add-hook 'typescript-mode-hook #'avm-typescript-mode-hook))
