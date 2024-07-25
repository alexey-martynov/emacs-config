(require 'cl-lib)

(defvar latex-aux-file nil "Auxiliary file for LaTeX source to provide labels")
(make-variable-buffer-local 'latex-aux-file)
(put 'latex-aux-file 'safe-local-variable 'stringp)

(defun latex-list-aux-labels ()
  "Return list of labels available from current buffer's auxiliary file."
  (when (and latex-aux-file
             (file-readable-p latex-aux-file))
    (let ((source latex-aux-file))
      (with-temp-buffer
        (insert-file-contents source)
        (let (labels)
          (goto-char (point-min))
          (while (re-search-forward "\\\\newlabel{\\([^}]*\\)}" nil t)
            (push (match-string 1) labels))
          labels)))))

(defun latex-list-buffer-labels (labels)
  "Return lis of labels available from current buffer appended to LABELS"
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "\\\\label{\\([^}]*\\)}" nil t)
      (cl-pushnew (match-string 1) labels))
    labels))

(defun latex-insert-reference-impl (command)
  "Insert reference via COMMAND.

If there is a highlighted region, the refernce COMMAND is wrapped
around the region text. If LATEX-AUX-FILE is set the list of
available labels is parsed from file and used as completion in
interactive prompt. If there is no auxiliary file and now highlighted
region the COMMAND with empty argument is inserted."
  (cond
   ((use-region-p)
    (save-excursion
      (let ((begin (region-beginning))
            (end (region-end)))
        (goto-char end)
        (insert "}")
        (goto-char begin)
        (insert "\\" command "{"))))
    (latex-aux-file
     (let ((label (completing-read "Label: " (latex-list-buffer-labels (latex-list-aux-labels)))))
       (insert "\\" command "{" label "}")))
    (t
     (insert "\\" command "{}")
     (backward-char))))

(defun latex-insert-reference ()
  "Insert reference."
  (interactive)
  (latex-insert-reference-impl "ref"))

(defun latex-insert-autoreference ()
  "Insert autoreference."
  (interactive)
  (latex-insert-reference-impl "autoref"))

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

  (setq-local show-trailing-whitespace t)
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
  )
