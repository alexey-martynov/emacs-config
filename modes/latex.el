(defun latex-insert-reference ()
  (interactive)
  (insert "\\ref{}")
  (backward-char))

(defun latex-insert-label ()
  (interactive)
  (case major-mode
    (c++-mode
     (insert "/*$\\label{}$*/")
     (backward-char 4))
    (c-mode
     (insert "/*$\\label{}$*/")
     (backward-char 4))
    (latex-mode
     (insert "\\label{}")
     (backward-char))))

(add-hook 'latex-mode-hook
          '(lambda ()
            (define-key latex-mode-map (kbd "<RET>") 'newline-and-indent)
            (define-key latex-mode-map (kbd "C-c r") 'latex-insert-reference)
            (define-key latex-mode-map (kbd "C-c l") 'latex-insert-label)
            (turn-on-auto-fill)
            (setq show-trailing-whitespace t)
            (delete-trailing-whitespace-mode 'clean)
            (local-set-key (kbd "<f7>") 'compile)
            ;; Turn on Russian typesetting features
            (setq-default tex-open-quote "<<")
            (setq-default tex-close-quote ">>")
            (add-to-list 'tex-verbatim-environments "cxxsource")
            (add-to-list 'tex-verbatim-environments "lispsource")
            ))
