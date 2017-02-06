(defun avm-html-mode-hook ()
  (define-key html-mode-map (kbd "<RET>") 'newline-and-indent)
  (define-key html-mode-map (kbd "C-c o") 'ff-find-other-file)
  (setq show-trailing-whitespace t)
  (delete-trailing-whitespace-mode 'clean)
  (mmm-mode-on)
  (add-to-list 'ff-other-file-alist
               '("\\.html$" (".js")))
  )

(add-hook 'html-mode-hook #'avm-html-mode-hook)
