(setq ispell-dictionary "ru")
(setq ispell-program-name "aspell")

(defun avm-commit-mode-aspell-hook ()
  (if (buffer-file-name (current-buffer))
      (if (member (file-name-nondirectory (buffer-file-name (current-buffer))) '("COMMIT_EDITMSG" "TAG_EDITMSG" "PULLREQ_EDITMSG"))
          (flyspell-mode t))))

(add-hook 'text-mode-hook #'avm-commit-mode-aspell-hook)
