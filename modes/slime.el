(when (or (locate-library "slime") (locate-library "~/quicklisp/slime-helper"))
  (when (locate-library "~/quicklisp/slime-helper")
    (load "~/quicklisp/slime-helper"))
  (require 'slime)
  ;; Configure Lisp implementations
  ;; NOTE: Clozure CL may requires  parameters "-K utf-8"
  (let ((impl-dir (file-name-concat (file-name-directory load-file-name)
                                    "lisp.d")))

    (when (file-exists-p impl-dir)
      (mapc #'(lambda (path)
                (when (and (not (file-directory-p path))
                           (file-executable-p path))
                  (add-to-list 'slime-lisp-implementations (list (intern (file-name-nondirectory path))
                                                                 (list path)))))
            (directory-files impl-dir t)))
    ;; TODO: Handle Windows
    (let ((hyperspec (file-name-concat impl-dir "HyperSpec")))
      (when (and (file-exists-p hyperspec)
                 (file-directory-p hyperspec))
        (setq common-lisp-hyperspec-root (concat "file://" hyperspec "/")))))
  (slime-setup '(slime-fancy slime-asdf))
  (when (locate-library "closure-template-html-mode")
    (add-to-list 'auto-mode-alist
                 '("\\.tmpl$" . closure-template-html-mode)))
  (when (locate-library "w3m")
    (require 'hyperspec)
    (defun hyperspec-lookup (&optional symbol-name)
      (interactive)
      (let ((browse-url-browser-function 'w3m-browse-url))
        (if symbol-name
            (common-lisp-hyperspec symbol-name)
          (call-interactively 'common-lisp-hyperspec)))))
  (add-hook 'slime-mode-hook
            #'(lambda ()
                (local-set-key (kbd "<RET>") 'newline-and-indent)
                (delete-trailing-whitespace-mode 'clean)
                ;;(unless (slime-connected-p)
                ;;  (save-excursion (slime)))))
                )))

