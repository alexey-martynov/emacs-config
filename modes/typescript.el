(when (locate-library "scss-mode")
  (setf typescript-indent-level 2)
  (setf show-trailing-whitespace t)
  (delete-trailing-whitespace-mode 'clean)
  )