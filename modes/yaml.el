(when (locate-library "yaml-mode")
  (autoload 'yaml-mode "yaml-mode"
    "Major mode for editing YAML files" t)
  (add-to-list 'auto-mode-alist '("\\.ya?ml\\'" . yaml-mode))
  )
