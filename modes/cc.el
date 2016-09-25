(defun avm-c-mode-common-hook ()
  (local-set-key (kbd "<RET>") 'c-context-line-break)
  (local-set-key (kbd "\C-co") 'ff-find-other-file)
  (local-set-key (kbd "<f7>") 'compile)
  (local-set-key (kbd "C-c M-m") 'my-imenu)
  (local-set-key (kbd "C-c , s") 'semantic-analyze-proto-impl-toggle)
  (local-set-key (kbd "C-c l") 'latex-insert-label)
  (when (locate-library "smart-tabs-mode")
    ;; TODO: The value 2 should be obtained from the common source
    ;; as c-basic-offset
    (setq tab-width 2)
    (setq indent-tabs-mode t))
  (modify-syntax-entry ?_ "w")
  (setq show-trailing-whitespace t)
  (when (locate-library "gtags")
    (gtags-mode 1))
  (make-local-variable 'gtags-ignore-case)
  (setq gtags-ignore-case nil)
                                        ;(ede-minor-mode 1)
  (delete-trailing-whitespace-mode 'clean)
  )

(add-hook 'c-mode-common-hook #'avm-c-mode-common-hook)

