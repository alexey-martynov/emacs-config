(when (locate-library "cmake-mode")
  (autoload 'cmake-mode "cmake-mode" "CMake mode")

  (defun avm-cmake-mode-hook ()
    (local-set-key (kbd "<f7>") 'compile))

  (add-hook 'cmake-mode-hook #'avm-cmake-mode-hook)

  (setq auto-mode-alist
        (append
         '(("CMakeLists\\.txt\\'" . cmake-mode))
         '(("\\.cmake\\'" . cmake-mode))
         auto-mode-alist)))
