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
    (lisp-mode
     (insert ";$\\label{}$")
     (backward-char 2))
    (latex-mode
     (insert "\\label{}")
     (backward-char))))

(define-skeleton latex-insert-emph
  "Insert \emph"
  nil
  "\\emph{" _ "}")

(define-skeleton latex-insert-index
  "Insert \index"
  nil
  "\\index{" _ "}")

(defun avm-latex-mode-hook ()
  (define-key latex-mode-map (kbd "<RET>") 'newline-and-indent)
  (define-key latex-mode-map (kbd "C-c r") 'latex-insert-reference)
  (define-key latex-mode-map (kbd "C-c l") 'latex-insert-label)
  (turn-on-auto-fill)
  (setq show-trailing-whitespace t)
  (delete-trailing-whitespace-mode 'clean)
  (local-set-key (kbd "M-i") 'latex-insert-emph)
  (local-set-key (kbd "C-c M-i") 'latex-insert-index)
  (local-set-key (kbd "<f7>") 'compile)

  ;; Always turn on Flyspell outside Windows
  (unless running-windows
    (flyspell-mode t))

  ;; Turn on Russian typesetting features
  (setq-default tex-open-quote "<<")
  (setq-default tex-close-quote ">>")

  ;; Extend list of verbatim environments with
  ;; handcrafted envirinments for seminars
  (add-to-list 'tex-verbatim-environments "cxxsource")
  (add-to-list 'tex-verbatim-environments "lispsource")
  (add-to-list 'tex-verbatim-environments "lisprepl")
  (add-to-list 'tex-verbatim-environments "console"))

(add-hook 'latex-mode-hook #'avm-latex-mode-hook)
