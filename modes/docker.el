(when (locate-library "dockerfile-mode")
  (autoload 'dockerfile-mode "dockerfile-mode" "Dockerfile mode")

  (add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode)))
