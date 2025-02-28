(defun avm-lisp-data-mode-hook ()
  (setq-local show-trailing-whitespace t)
    (delete-trailing-whitespace-mode 'clean)
    ;;(unless (slime-connected-p)
    ;;  (save-excursion (slime)))))
    )

(add-hook 'lisp-data-mode-hook #'avm-lisp-data-mode-hook)

(when (locate-library "~/quicklisp/slime-helper")
  (load "~/quicklisp/slime-helper"))

(use-package slime
  :if (locate-library "slime")
  :commands (slime lisp-mode)
  :init
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
            (directory-files impl-dir t))))
    ;; TODO: Handle Windows
    ;;(let ((hyperspec (file-name-concat impl-dir "HyperSpec")))
    ;;  (when (and (file-exists-p hyperspec)
    ;;            (file-directory-p hyperspec))
    ;;   (setq common-lisp-hyperspec-root (concat "file://" hyperspec "/")))))
  :config
  (slime-setup '(slime-fancy slime-asdf))
  :bind (:map lisp-mode-map
              ("C-c l" . latex-insert-label)
              ("<RET>" . newline-and-indent))
  ;;(when (locate-library "w3m")
  ;;  (require 'hyperspec)
  ;;  (defun hyperspec-lookup (&optional symbol-name)
  ;;    (interactive)
  ;;    (let ((browse-url-browser-function 'w3m-browse-url))
  ;;      (if symbol-name
  ;;          (common-lisp-hyperspec symbol-name)
  ;;        (call-interactively 'common-lisp-hyperspec)))))
  )

(defun avm-closure-template-mode-hook ()
  (setq-local show-trailing-whitespace t)
  (delete-trailing-whitespace-mode 'clean)
  )

(use-package closure-template-html-mode
  :if (locate-library "closure-template-html-mode")
  :mode ("\\.tmpl$" . closure-template-html-mode)
  :bind (:map closure-template-html-mode-map
              ("C-c C-l" . closure-template-compile))
  :hook (closure-template-html-mode-hook . avm-closure-template-mode-hook))
