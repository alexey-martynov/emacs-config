(defun avm-egg-commit-buffer-mode-hook ()
  (auto-fill-mode t)
  (unless running-windows
    (flyspell-mode)
  ))

(when (locate-library "egg")
  (require 'egg)
  (add-hook 'egg-commit-buffer-mode-hook #'avm-egg-commit-buffer-mode-hook))
