(when (locate-library "restclient")
  (autoload 'restclient-mode "restclient-mode" "REST Client mode")

  (add-to-list 'auto-mode-alist '("\\.rest\\'" . restclient-mode)))
