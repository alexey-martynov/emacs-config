(defun avm-haskell-mode-hook ()
  ;;(define-key haskell-mode-map [?\C-c ?\C-r] 'inferior-haskell-reload-file)
  (message "Running Haskell hooks")
  (setq show-trailing-whitespace t)
  (delete-trailing-whitespace-mode 'clean)
  (interactive-haskell-mode))

(add-hook 'haskell-mode-hook #'avm-haskell-mode-hook)
