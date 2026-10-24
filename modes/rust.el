(defun avm-rust-mode-hook ()
  (local-set-key (kbd "<f7>") 'compile)

  (setq-local show-trailing-whitespace t)
  (delete-trailing-whitespace-mode 'clean)
  )

(add-hook 'rust-mode-hook #'avm-rust-mode-hook)
