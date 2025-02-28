(require 'cl-lib)

(defvar latex-aux-file nil "Auxiliary file for LaTeX source to provide labels")
(make-variable-buffer-local 'latex-aux-file)
(put 'latex-aux-file 'safe-local-variable 'stringp)

(defun latex-list-aux-labels* ()
  "Return list of labels available from current buffer.

The returned list is list of CONS cells where CAR is a label and CDR is a caption."
  (let (labels)
    (goto-char (point-min))
    (while (re-search-forward "\\\\newlabel{\\([^}]*\\)}" nil t)
      (let* ((label (match-string 1))
             (begin (match-end 0))
             (end (condition-case error
                      (scan-sexps begin 1)
                    (scan-error nil))))
        (when end
          ;; Obtain 3rd element
          (setf begin (condition-case error
                          (scan-sexps (1+ begin) 1)
                        (scan-error nil)))
          (when begin
            (setf begin (condition-case error
                            (scan-sexps (1+ begin) 1)
                          (scan-error nil)))
            (when begin
              (let ((pos (condition-case error
                             (scan-sexps (1+ begin) 1)
                           (scan-error nil))))
                (when pos
                  (push (cons label (buffer-substring-no-properties (+ 2 begin) (1- pos))) labels))))))
          (goto-char (1+ end))))
    labels))

(defun latex-list-aux-labels ()
  "Return list of labels available from current buffer's auxiliary file.

The returned list is list of CONS cells where CAR is a label and CDR is a caption."
  (when (and latex-aux-file
             (file-readable-p latex-aux-file))
    (let ((source latex-aux-file))
      (with-temp-buffer
        (insert-file-contents source)
        (latex-list-aux-labels*)))))

(defun latex-list-buffer-labels (labels)
  "Return list of labels available from current buffer appended to LABELS."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "\\\\label{\\([^}]*\\)}" nil t)
      (cl-pushnew (match-string 1) labels))
    labels))

(defun latex-label-info> (a b)
  "Compare latex label info"
  (let ((s1 (if (stringp a) a (car a)))
        (s2 (if (stringp b) b (car b))))
    (string> s1 s2)))

(defun latex-imenu-create-label-index (document-labels)
  "Generate Imenu content from DOCUMENT-LABELS.

All labels are split to category submenus according to to prefix: 'chap',
'sec', 'lst', 'fig' and 'tbl'. Uncategorized labels are placed to 'Other'
submenu. The listings has additional level: if label has 2 prefixes a submenu
of listing is created and label placed there."
  (let ((labels (sort document-labels #'latex-label-info>))
        chapters
        sections
        listings
        figures
        tables
        other
        (result '(("*Empty*" . "")))
        current-listing-id
        current-listing-caption
        current-listing-lines
        current-listing-label
        last-label)
    (dolist (item labels)
      (let* ((label (if (stringp item) item (car item)))
             (parts (split-string label ":" t "\s"))
             caption
             (type (car parts)))
        (unless (string= last-label label)
          (setf last-label label)
          (if (stringp item)
              ;; Label without caption
              (setf caption (if (cdr parts) (cadr parts) label))
            ;; Label with caption
            (setf caption (cdr item)))
          (cond
           ((string= "chap" type)
            (push (cons (format "%s (%s)" caption (cadr parts)) label) chapters))
           ((string= "sec" type)
            (push (cons (format "%s (%s)" caption (cadr parts)) label) sections))
           ((string= "lst" type)
            (if (cddr parts)
                ;; Has line labels
                (let ((id (cadr parts)))
                  (if (string= current-listing-id id)
                      ;; Continue line collecting
                      (push (cons (caddr parts) label) current-listing-lines)
                    (progn
                      ;; New listing id
                      (when current-listing-lines
                        (push (cons "*Listing*" (concat "lst:" current-listing-id)) current-listing-lines)
                        (push (cons current-listing-id current-listing-lines) listings))
                      (setf current-listing-id id
                            current-listing-label label
                            current-listing-caption (format "%s (%s)" caption id)
                            current-listing-lines (list (cons (caddr parts) label))))))
              (progn
                ;; No line numbers
                (when current-listing-lines
                  ;; Current listing name matches collected line numbers,
                  ;; add special reference
                  (push (cons "*Listing*" (concat "lst:" current-listing-id)) current-listing-lines)
                  (push (cons current-listing-caption current-listing-lines) listings))
                (unless (string= current-listing-id (cadr parts))
                  (push (cons (format "%s (%s)" caption (cadr parts)) label) listings))
                (setf current-listing-id nil
                      current-listing-caption nil
                      current-listing-lines nil
                      current-listing-label nil))))
           ((string= "fig" type)
            (push (cons (format "%s (%s)" caption (cadr parts)) label) figures))
           ((string= "tbl" type)
            (push (cons (format "%s (%s)" caption (cadr parts)) label) tables))
           (t
            (push (cons (format "%s (%s)" caption label) label) other))))))
    (when current-listing-lines
      ;; Pick up uncompleted listing line list
      (push (cons (or current-listing-caption current-listing-id) current-listing-lines) listings))
    (when other
      (push (cons "Other" other) result))
    (when tables
      (push (cons "Tables" tables) result))
    (when figures
      (push (cons "Figures" figures) result))
    (when listings
      (push (cons "Listings" listings) result))
    (when sections
      (push (cons "Sections" sections) result))
    (when chapters
      (push (cons "Chapters" chapters) result))
    result))

(defun avm-latex-imenu-label-helper (command)
  (let (index-alist
        (result t))
    ;; Create a list for this buffer only when needed.
    (while (eq result t)
      (setf index-alist (latex-imenu-create-label-index (latex-list-buffer-labels (latex-list-aux-labels))))
      (setf result (imenu--mouse-menu index-alist t))
      (cond
       ((equal result imenu--rescan-item)
        (progn
          (imenu--cleanup)
          (setq result t
                imenu--index-alist nil)))
       ((consp result)
        (insert "\\" command "{" (cdr result) "}")))
    result)))

(defun latex-insert-reference-impl (command)
  "Insert reference via COMMAND.

If there is a highlighted region, the reference COMMAND is wrapped
around the region text. If LATEX-AUX-FILE is set the list of
available labels is parsed from file and used as completion in
interactive prompt or Imenu. If there is no auxiliary file and no
highlighted region the COMMAND with empty argument is inserted."
  (cond
   ((use-region-p)
    (save-excursion
      (let ((begin (region-beginning))
            (end (region-end)))
        (goto-char end)
        (insert "}")
        (goto-char begin)
        (insert "\\" command "{"))))
   ((and latex-aux-file window-system)
    (imenu (avm-latex-imenu-label-helper command)))
   (latex-aux-file
    (let ((label (completing-read "Label: " (mapcar #'(lambda (item)
                                                        (if (stringp item) item (car item)))
                                                    (latex-list-buffer-labels (latex-list-aux-labels))))))
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
                     (scheme-mode
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
