(setq delete-trailing-whitespace-mode nil)
(make-variable-buffer-local 'delete-trailing-whitespace-mode)
(put 'delete-trailing-whitespace-mode 'safe-local-variable #'(lambda (val) (or (eq val t) (eq val nil))))

(defcustom delete-trailing-whitespace-keep-exts
  '("gif" "png" "jpeg" "jpg")
  "List of file extensions int which traling whitespace cleanup never performs")

(defun delete-trailing-whitespace-on-save ()
  (if (buffer-file-name)
      (if (and
           (buffer-local-value 'delete-trailing-whitespace-mode (current-buffer))
           (not (member (file-name-nondirectory (buffer-file-name)) '("COMMIT_EDITMSG" "TAG_EDITMSG")))
           (let ((ext (file-name-extension (buffer-file-name))))
             (cond
              (ext
               (not (member-ignore-case ext delete-trailing-whitespace-keep-exts)))
              (t
               t)))
           (save-excursion (goto-char (point-min))
                           (re-search-forward "[[:blank:]]$" nil t)))
          (delete-trailing-whitespace))))

(defun delete-trailing-whitespace-mode (&optional arg)
  (interactive "P")
  (prog1 (setq delete-trailing-whitespace-mode
               (if (null arg) (not delete-trailing-whitespace-mode)
                 (if (eq arg 'clean)
                     (not (save-excursion (goto-char (point-min))
                                          (re-search-forward "[[:blank:]]$" nil t)))
                   (> (prefix-numeric-value arg) 0))))))

(add-to-list 'minor-mode-alist '(delete-trailing-whitespace-mode " TWS"))

(add-hook 'before-save-hook 'delete-trailing-whitespace-on-save)

(provide 'delete-trailing-whitespace-mode)
