(when (locate-library "restclient")
  (autoload 'restclient-mode "restclient" "REST Client mode" t)

  (add-to-list 'auto-mode-alist '("\\.rest\\'" . restclient-mode)))
