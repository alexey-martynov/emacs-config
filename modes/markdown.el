(defun avm-markdown-mode-hook ()
  (delete-trailing-whitespace-mode 'clean)
  (setq show-trailing-whitespace t)

  (auto-fill-mode t)
  (flyspell-mode t)
  )

(when (locate-library "markdown-mode")
  (autoload 'markdown-mode "markdown-mode"
    "Major mode for editing Markdown files" t)


  (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

  (autoload 'gfm-mode "markdown-mode"
    "Major mode for editing GitHub Flavored Markdown files" t)
  (add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))

  (add-hook 'markdown-mode-hook #'avm-markdown-mode-hook))
