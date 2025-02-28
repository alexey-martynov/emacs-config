(defun avm-scheme-mode-hook ()
  (setq-local show-trailing-whitespace t)
  (delete-trailing-whitespace-mode 'clean)
  (define-key scheme-mode-map (kbd "C-c l") 'latex-insert-label)
  )

(add-hook 'scheme-mode-hook #'avm-scheme-mode-hook)
