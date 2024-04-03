(defun avm-haskell-mode-hook ()
  ;;(define-key haskell-mode-map [?\C-c ?\C-r] 'inferior-haskell-reload-file)
  (define-key haskell-mode-map (kbd "<f7>") 'compile)
  (define-key haskell-mode-map (kbd "C-c M-m") 'my-imenu)

  (setq show-trailing-whitespace t)
  (delete-trailing-whitespace-mode 'clean)
  (interactive-haskell-mode))

(add-hook 'haskell-mode-hook #'avm-haskell-mode-hook)
