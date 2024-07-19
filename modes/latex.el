(define-skeleton latex-insert-reference
  "Insert \\ref"
  nil
  "\\ref{" _ "}")

(define-skeleton latex-insert-autoreference
  "Insert \\autoref"
  nil
  "\\autoref{" _ "}")

(defun latex-insert-label ()
  (interactive)
  (cl-case major-mode
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
  "Insert \\emph"
  nil
  "\\emph{" _ "}")

(define-skeleton latex-insert-index
  "Insert \\index"
  nil
  "\\index{" _ "}")

(define-skeleton latex-insert-footnote
  "Insert \\footnote"
  nil
  "\\footnote{" _ "}")

(defun avm-latex-mode-hook ()
  (define-key latex-mode-map (kbd "<RET>") #'newline-and-indent)
  (define-key latex-mode-map (kbd "C-c r") #'latex-insert-reference)
  (define-key latex-mode-map (kbd "C-c C-r") #'latex-insert-autoreference)
  (define-key latex-mode-map (kbd "C-c l") #'latex-insert-label)
  (define-key latex-mode-map (kbd "C-c f") #'latex-insert-footnote)
  (define-key latex-mode-map (kbd "M-i") #'latex-insert-emph)
  (define-key latex-mode-map (kbd "C-c M-i") #'latex-insert-index)
  (define-key latex-mode-map (kbd "<f7>") #'compile)

  (turn-on-auto-fill)

  (setq show-trailing-whitespace t)
  (delete-trailing-whitespace-mode 'clean)

  ;; Always turn on Flyspell outside Windows
  (unless running-windows
    (flyspell-mode t))

  ;; Turn on Russian typesetting features
  (setopt tex-open-quote "<<")
  (setopt tex-close-quote ">>")

  ;; Prevent subscript/superscript formatting
  (setopt tex-fontify-script nil)

  ;; Extend list of verbatim environments with
  ;; handcrafted envirinments for seminars
  (add-to-list 'tex-verbatim-environments "cxxsource")
  (add-to-list 'tex-verbatim-environments "lispsource")
  (add-to-list 'tex-verbatim-environments "lisprepl")
  (add-to-list 'tex-verbatim-environments "console"))

(add-hook 'latex-mode-hook #'avm-latex-mode-hook)
