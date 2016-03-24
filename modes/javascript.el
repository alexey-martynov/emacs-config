(defun avm-js-mode-hook ()
  (define-key js-mode-map (kbd "<RET>") 'newline-and-indent)
  (setq show-trailing-whitespace t)
  (electric-indent-mode t)
  (delete-trailing-whitespace-mode 'clean)
  (fic-mode)
  )

(add-hook 'js-mode-hook #'avm-js-mode-hook)
