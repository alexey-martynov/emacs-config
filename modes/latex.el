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
  (turn-on-auto-fill)

  (delete-trailing-whitespace-mode 'clean)

  ;; Always turn on Flyspell outside Windows
  (unless running-windows
    (flyspell-mode t))
  )

(use-package tex-mode
  :defer t
  :config (add-to-list 'tex-verbatim-environments "cxxsource")
          (add-to-list 'tex-verbatim-environments "lispsource")
          (add-to-list 'tex-verbatim-environments "lisprepl")
          (add-to-list 'tex-verbatim-environments "console")
  :bind (:map latex-mode-map
              ("<RET>" . newline-and-indent)
              ("C-c r" . latex-insert-reference)
              ("C-c C-r" . latex-insert-autoreference)
              ("C-c l" . latex-insert-label)
              ("C-c f" . latex-insert-footnote)
              ("M-i" . latex-insert-emph)
              ("C-c M-i" . latex-insert-index)
              ("<f7>" . compile))
  :hook (latex-mode . avm-latex-mode-hook)
  :custom (tex-open-quote "<<" "Turn on Russian typesetting features")
          (tex-close-quote ">>" "Turn on Russian typesetting features")
          (tex-fontify-script nil "Prevent subscript/superscript formatting")
          (show-trailing-whitespace t)
  )
