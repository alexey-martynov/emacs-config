(defconst c-martynov-style
  '("bsd"
    (c-basic-offset . 2)
    (c-cleanup-list . (scope-operator
                       brace-else-brace
                       brace-elseif-brace
                       brace-catch-brace
                       empty-defun-braces))
    (c-hanging-braces-alist . ((block-close . c-snug-do-while)
                               (statement-cont)
                               (substatement-open after)
                               (brace-list-open)
                               (brace-entry-open)
                               (extern-lang-open after)
                               (namespace-open after)
                               (namespace-close)
                               (module-open after)
                               (composition-open after)
                               (inexpr-class-open after)
                               (inexpr-class-close before)
                               (arglist-cont-nonempty)))
    (c-offsets-alist . ((arglist-intro . ++)
                        (arglist-cont . 0)
                        (arglist-cont-nonempty . ++)
                        (inher-intro . ++)
                        (member-init-cont . 0)
                        (statement-cont . ++)
                        (substatement-open . 0)))
    (c-require-final-newline . ((c-mode . t)
                                (c++-mode . t)
                                (objc-mode . t)
                                (java-mode . t)
                                (erlang-mode . t)
                                (haskell-mode . t)
                                (html-mode . t))))
    "Alexey Martynov coding style")
(c-add-style "martynov" c-martynov-style)

(custom-set-variables
 '(c-default-style (quote ((java-mode . "java")
                           (awk-mode . "awk")
                           (other . "martynov")))))

(defun avm-c-mode-common-hook ()
  (local-set-key (kbd "<RET>") 'c-context-line-break)
  (local-set-key (kbd "\C-co") 'ff-find-other-file)
  (local-set-key (kbd "<f7>") 'compile)
  (local-set-key (kbd "C-c M-m") 'my-imenu)
  (local-set-key (kbd "C-c , s") 'semantic-analyze-proto-impl-toggle)
  (local-set-key (kbd "C-c l") 'latex-insert-label)
  (when (locate-library "smart-tabs-mode")
    ;; TODO: The value 2 should be obtained from the common source
    ;; as c-basic-offset
    (setq tab-width 2)
    (setq indent-tabs-mode t))
  (modify-syntax-entry ?_ "w")

  (setq-local show-trailing-whitespace t)
  (delete-trailing-whitespace-mode 'clean)

  (when (locate-library "gtags")
    (gtags-mode 1))
  (make-local-variable 'gtags-ignore-case)
  (setq gtags-ignore-case nil)
                                        ;(ede-minor-mode 1)
  )

(add-hook 'c-mode-common-hook #'avm-c-mode-common-hook)
