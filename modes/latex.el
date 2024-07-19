(define-skeleton latex-insert-reference
  "Insert \\ref"
  nil
  "\\ref{" _ "}")

(define-skeleton latex-insert-autoreference
  "Insert \\autoref"
  nil
  "\\autoref{" _ "}")

(defun latex-insert-label ()
  "Insert LaTeX label definition depending on current major mode.

If there is a highlighted region, the label command is wrapped
around the region text."
  (interactive)
  (let ((insertion (cl-case major-mode
                     (c++-mode
                      '("/*$\\label{" . "}$*/"))
                     (c-mode
                      '("/*$\\label{" . "}$*/"))
                     (lisp-mode
                      '(";$\\label{" . "}$"))
                     (latex-mode
                      '("\\label{" . "}")))))
    (when insertion
      (if (use-region-p)
          (save-excursion
            (let ((begin (region-beginning))
                  (end (region-end)))
              (goto-char end)
              (insert (cdr insertion))
              (goto-char begin)
              (insert (car insertion))))
        (progn
          (insert (car insertion) (cdr insertion))
          (backward-char (length (cdr insertion))))))))

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
