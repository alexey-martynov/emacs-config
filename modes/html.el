(defun avm-html-mode-hook ()
  (define-key html-mode-map (kbd "<RET>") 'newline-and-indent)
  (setq show-trailing-whitespace t)
  (delete-trailing-whitespace-mode 'clean)
  (mmm-mode-on)
  )

(add-hook 'html-mode-hook #'avm-html-mode-hook)
