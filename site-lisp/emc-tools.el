(defun emc-c++-generate-content-by-members (item-generator &optional finalizer)
  (save-excursion
    (let* ((begin (region-beginning))
           (end (region-end))
           (line-begin (progn (goto-char begin)
                             (if (bolp)
                                 begin
                               (line-beginning-position))))
          (line-end (progn (goto-char end)
                           (if (eolp)
                               end
                             (line-end-position))))
          (first t))
      (goto-char line-begin)
      (let ((marker (make-marker)))
        (set-marker-insertion-type marker t)
        (set-marker marker line-end)
        (while (< (point) (marker-position marker))
          (let ((line (replace-regexp-in-string "\\`[ \t\n]*" ""
                                                (replace-regexp-in-string "[ \t\n]*\\'" ""
                                                                          (buffer-substring-no-properties (point) (line-end-position))))))
            (delete-region (point) (line-end-position))
            (apply #'insert (funcall item-generator line first))
            (setf first nil))
          (forward-line 1)
          )
        (backward-char)
        (when (and finalizer (or (stringp finalizer) (functionp finalizer)))
          (if (stringp finalizer)
              (insert  finalizer)
            (funcall finalizer)))
        (indent-region line-begin (marker-position marker))))))

(defun emc-c++-generate-equality-operator ()
  (interactive)
  (emc-c++-generate-content-by-members #'(lambda (member first)
                                       (list (if first "return " "&& ") "(" line " == rhs." line ")"))
                                   ";"))

(defun emc-c++-generate-copy-constructor ()
  (interactive)
  (emc-c++-generate-content-by-members #'(lambda (member first)
                                       (list line "(rhs." line "),"))
                                   #'(lambda ()
                                       (delete-char -1))))


(defun emc-update-copyright-years ()
  (when (buffer-file-name)
    (interactive)
    (save-excursion
      (goto-char (point-min))
      (let ((regex (concat comment-start-skip "COPYRIGHT\\s-+(C)\\s-EMC[^[:digit:]]**\\(?8:[[:digit:]]\\{4\\}-\\)?\\(?9:[[:digit:]]\\{4\\}\\)")))
        (when (re-search-forward regex nil t 1)
          (let ((current-year (nth 5 (decode-time)))
                (year (string-to-number (match-string-no-properties 9) 10)))
            (when (< year current-year)
              (if (match-string 8)
                  (replace-match (number-to-string current-year) t nil nil 9)
                (replace-match (concat "\\9-" (number-to-string current-year)) t nil nil 9)))))))))

(add-hook 'before-save-hook 'emc-update-copyright-years)

(provide 'emc-tools)
