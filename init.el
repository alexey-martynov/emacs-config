;; -*- mode: emacs-lisp -*-
(defvar running-windows (eq system-type 'windows-nt))
(defvar running-x (eq window-system 'x))
(defvar running-mac (eq window-system 'ns))

(when (file-exists-p "~/.emacs.d/site-lisp")
  (add-to-list 'load-path "~/.emacs.d/site-lisp")
  (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
      (let* ((my-lisp-dir "~/.emacs.d/site-lisp/")
             (default-directory my-lisp-dir))
        (setq load-path (cons my-lisp-dir load-path))
        (normal-top-level-add-subdirs-to-load-path))))

(load "cyrillic-without-yo.el")

(when running-mac
  (set-input-method 'russian-computer-without-yo)
  (deactivate-input-method)
  (setenv "PATH" (concat "/usr/local/bin:" (getenv "PATH") ":/Library/TeX/texbin"))
  (setq exec-path (append '("/usr/local/bin") exec-path '("/Library/TeX/texbin"))))

(when running-windows
  (set-input-method 'russian-computer-without-yo)
  (deactivate-input-method)
  (setenv "PATH" (concat (getenv "PATH") ";C:\\texlive\\2014\\bin\\win32;C:\\MinGW\\bin;C:\\MinGW\\msys\\1.0\\bin"))
  (setq exec-path (append exec-path '("C:/texlive/2014/bin/win32" "C:/MinGW/bin" "C:/MinGW/msys/1.0/bin")))
  (prefer-coding-system 'windows-1251)
  (prefer-coding-system 'utf-8))

(when running-windows
  (set-default-font "DejaVu Sans Mono-11")
  (add-to-list 'default-frame-alist
               '(font . "DejaVu Sans Mono-11")))

(when running-mac
  (set-fontset-font t 'cyrillic (font-spec :name "Monaco")))

(require 'custom)
(when (file-exists-p "~/.emacs.d/themes")
  (add-to-list 'custom-theme-load-path "~/.emacs.d/themes"))

(load-theme 'twilight t)

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(blink-cursor-mode nil)
 '(c-basic-offset 2)
 '(c-cleanup-list (quote (scope-operator
                          brace-else-brace
                          brace-elseif-brace
                          brace-catch-brace
                          empty-defun-braces)))
 '(c-default-style (quote ((java-mode . "java")
                           (awk-mode . "awk")
                           (other . "bsd"))))
 '(c-echo-syntactic-information-p nil)
 '(c-hanging-braces-alist (quote ((block-close . c-snug-do-while)
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
                                  (arglist-cont-nonempty))))
 '(c-offsets-alist (quote ((arglist-intro . ++)
                           (arglist-cont . 0)
                           (arglist-cont-nonempty . ++)
                           (inher-intro . ++)
                           (member-init-cont . 0)
                           (statement-cont . ++)
                           (substatement-open . 0))))
 '(c-require-final-newline (quote ((c-mode . t)
                                   (c++-mode . t)
                                   (objc-mode . t)
                                   (java-mode . t)
                                   (erlang-mode . t)
                                   (haskell-mode . t)
                                   (html-mode . t))))
 '(column-number-mode t)
 '(compilation-read-command nil)
 '(cua-auto-tabify-rectangles nil)
 '(delete-selection-mode t)
 '(font-lock-mode t t (font-lock))
 '(imenu-sort-function 'imenu--sort-by-name)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(initial-scratch-message nil)
 '(pulse-flag (quote never))
 '(require-final-newline t)
 '(scroll-bar-mode (quote right))
 '(scroll-conservatively 1000000)
 '(scroll-preserve-screen-position 1)
 '(scroll-step 1)
 '(show-paren-mode t)
 '(show-paren-style (quote expression))
 '(toolbar-visible-p nil)
 '(transient-mark-mode t)
 '(truncate-lines t)
 '(truncate-partial-width-windows nil)
 '(visible-bell t)
 '(which-function-mode t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(tex-verbatim ((t (:inherit nil)))))

(fset 'yes-or-no-p 'y-or-n-p)
(setq make-backup-files nil)
(setq split-width-threshold most-positive-fixnum)
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

(if running-x
    (global-set-key [f13] 'toggle-input-method))

(icomplete-mode 99)

(windmove-default-keybindings 'meta)
(unless (or running-windows running-x)
  (global-set-key (kbd "ESC <left>") 'windmove-left)
  (global-set-key (kbd "ESC <right>") 'windmove-right)
  (global-set-key (kbd "ESC <down>") 'windmove-down)
  (global-set-key (kbd "ESC <up>") 'windmove-up))

(unless window-system
  (xterm-mouse-mode t))

;; Auto indent yanked code
(dolist (command '(yank yank-pop))
  (eval `(defadvice ,command (after indent-region activate)
           (and (not current-prefix-arg)
                (member major-mode '(emacs-lisp-mode lisp-mode
                                                     clojure-mode    scheme-mode
                                                     haskell-mode    ruby-mode
                                                     rspec-mode      python-mode
                                                     c-mode          c++-mode
                                                     objc-mode       ;latex-mode
                                                     plain-tex-mode))
                (let ((mark-even-if-inactive transient-mark-mode))
                  (indent-region (region-beginning) (region-end) nil))))))

(push '("<\\?xml\\b[^>]*\\bencoding=\"utf-8\"[^>]*\\?>" . utf-8) auto-coding-regexp-alist)

(when running-windows
  (defun insert-uuid ()
    "Insert UUID in upper case to current buffer using `uuidgen' for generation."
    (interactive)
    (save-excursion
      (if mark-active (kill-region (mark) (point)))
      (shell-command "uuidgen -c" t)
      (forward-char 36)
      (delete-char 1)))
  (define-key global-map "\C-cg" 'insert-uuid))

(require 'find-file)

(setq ff-other-file-alist
      '(("\\.cpp$" (".hpp" ".h"))
        ("\\.c$" (".h" ".hpp"))
        ("\\.hpp$" (".cpp" ".c"))
        ("\\.h$" (".cpp" ".c"))))
(put 'ff-search-directories 'safe-local-variable 'listp)

(setf completion-ignored-extensions
      (append completion-ignored-extensions
              '(".hi" ".pdf")))

(when (file-exists-p "~/.emacs.d/modes")
       (mapc #'load (directory-files "~/.emacs.d/modes" t "\\.el$")))

(require 'compile)
(setq compilation-scroll-output t)
(put 'compile-command 'safe-local-variable 'stringp)
(setenv "MAKEFLAGS" "-w")

(defvar compile-target nil)
(make-variable-buffer-local 'compile-target)
(put 'compile-target 'safe-local-variable 'stringp)
(defun compile (command &optional comint)
  "Compile the program including the current buffer.  Default: run `make'.
Runs COMMAND, a shell command, in a separate process asynchronously
with output going to the buffer `*compilation*'.

Patched by Alexey Martynov: if variable `compile-target' is defined it is
appended as the final argument to the `compile-command'. This allows to
specify a generic compile command like `make' and locally override target.

You can then use the command \\[next-error] to find the next error message
and move to the source code that caused it.

If optional second arg COMINT is t the buffer will be in Comint mode with
`compilation-shell-minor-mode'.

Interactively, prompts for the command if the variable
`compilation-read-command' is non-nil; otherwise uses `compile-command'.
With prefix arg, always prompts.
Additionally, with universal prefix arg, compilation buffer will be in
comint mode, i.e. interactive.

To run more than one compilation at once, start one then rename
the \`*compilation*' buffer to some other name with
\\[rename-buffer].  Then _switch buffers_ and start the new compilation.
It will create a new \`*compilation*' buffer.

On most systems, termination of the main compilation process
kills its subprocesses.

The name used for the buffer is actually whatever is returned by
the function in `compilation-buffer-name-function', so you can set that
to a function that generates a unique name."
  (interactive
   (list
    (let ((command (eval compile-command)))
      (if (or compilation-read-command current-prefix-arg)
	  (compilation-read-command command)
	command))
    (consp current-prefix-arg)))
  (unless (equal command (eval compile-command))
    (setq compile-command command))
  (when compile-target
    (setq command (concat command " " compile-target)))
  (save-some-buffers (not compilation-ask-about-save)
                     compilation-save-buffers-predicate)
  (setq-default compilation-directory default-directory)
  (compilation-start command comint))
(put 'compile-target 'safe-local-variable 'stringp)
(put 'eval 'safe-local-variable 'listp)

;;; Boost.Test fatal errors
(add-to-list 'compilation-error-regexp-alist '("^\\(.+\\)(\\([[:digit:]]+\\)):[[:space:]]*\\(?:fatal \\)?error" 1 2 nil 2))

(when (locate-library "mmm-mode")
  (require 'mmm-mode)
  (setq mmm-global-mode 'maybe)
  (when (locate-library "php-mode")
    (mmm-add-mode-ext-class nil "\\.php3?\\'" 'html-php)
    (mmm-add-classes
     '((html-php
        :submode php-mode
        :front "<\\?\\(php\\)?"
        :back "\\?>"))))
  (mmm-add-classes
   '((embedded-erlang
      :submode erlang-mode
      :front "<erl>"
      :back "</erl>")))
  (set-variable 'mmm-global-classes '(universal embedded-erlang)))

(when (locate-library "delete-trailing-whitespace-mode")
  (require 'delete-trailing-whitespace-mode))

(when (locate-library "sgml-mode")
  (require 'sgml-mode))

(when (locate-library "gtags")
  (setq gtags-suggested-key-mapping t)
  (setq gtags-disable-pushy-mouse-mapping t)
  (autoload 'gtags-mode "gtags" "" t))

(defun my-imenu-helper()
  (let (index-alist
        (result t)
        alist)
    ;; Create a list for this buffer only when needed.
    (while (eq result t)
      (setq index-alist (imenu--make-index-alist))
      (setq result (imenu--mouse-menu index-alist t))
      (and (equal result imenu--rescan-item)
           (imenu--cleanup)
           (setq result t imenu--index-alist nil)))
    result))

(defun my-imenu() (interactive) (imenu (my-imenu-helper)))

(when (locate-library "fic-mode")
  (require 'fic-mode)
  (add-hook 'prog-mode-hook 'fic-mode)
  (push '(fic-face . font-lock-warning-face) face-remapping-alist)
  (push "HACK" fic-highlighted-words))

(add-hook 'emacs-lisp-mode-hook
          '(lambda ()
             (setq show-trailing-whitespace t)
             (delete-trailing-whitespace-mode 'clean)
             ))

;; CC-mode
(when (locate-library "smart-tabs-mode")
  (require 'smart-tabs-mode)
  (defadvice align (around smart-tabs activate)
    (let ((indent-tabs-mode nil)) ad-do-it))
  (defadvice align-regexp (around smart-tabs activate)
    (let ((indent-tabs-mode nil)) ad-do-it))
  (defadvice indent-relative (around smart-tabs activate)
    (let ((indent-tabs-mode nil)) ad-do-it))
  (defadvice indent-according-to-mode (around smart-tabs activate)
    (let ((indent-tabs-mode indent-tabs-mode))
      (if (memq indent-line-function
                '(indent-relative
                  indent-relative-maybe))
          (setq indent-tabs-mode nil))
      ad-do-it))
  (defmacro smart-tabs-advice (function offset)
    `(progn
       (defvaralias ',offset 'tab-width)
       (defadvice ,function (around smart-tabs activate)
         (cond
          (indent-tabs-mode
           (save-excursion
             (beginning-of-line)
             (while (looking-at "\t*\\( +\\)\t+")
               (replace-match "" nil nil nil 1)))
           (setq tab-width tab-width)
           (let ((tab-width fill-column)
                 (,offset fill-column)
                 (wstart (window-start)))
             (unwind-protect
                 (progn ad-do-it)
                 ;(set-window-start (selected-window) wstart)
               )))
          (t
           ad-do-it)))))
  (smart-tabs-advice c-indent-line c-basic-offset)
  (smart-tabs-advice c-indent-region c-basic-offset))


(add-hook 'c-mode-common-hook
          '(lambda ()
             (local-set-key (kbd "<RET>") 'newline-and-indent)
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
             (setq show-trailing-whitespace t)
             (when (locate-library "gtags")
               (gtags-mode 1))
             (make-local-variable 'gtags-ignore-case)
             (setq gtags-ignore-case nil)
             ;(ede-minor-mode 1)
             (delete-trailing-whitespace-mode 'clean)
             ))

(add-hook 'makefile-mode-hook
          '(lambda ()
             (local-set-key (kbd "<f7>") 'compile)
             (modify-syntax-entry ?_ "w")
             (delete-trailing-whitespace-mode 'clean)
             ))

(add-hook 'compilation-mode-hook
          '(lambda ()
             (local-set-key (kbd "<f7>") 'recompile)))

(add-hook 'python-mode-hook
          '(lambda ()
             (define-key python-mode-map (kbd "<RET>") 'newline-and-indent)
             (modify-syntax-entry ?_ "w")
             (setq show-trailing-whitespace t)
             (delete-trailing-whitespace-mode 'clean)
             ))

(add-hook 'haskell-mode-hook
          '(lambda ()
             (define-key haskell-mode-map [?\C-c ?\C-r] 'inferior-haskell-reload-file)
             (setq show-trailing-whitespace t)
             (delete-trailing-whitespace-mode 'clean)
             ))

(add-hook 'erlang-mode-hook
          '(lambda ()
             (define-key erlang-mode-map (kbd "<RET>") 'newline-and-indent)
             (setq show-trailing-whitespace t)
             (delete-trailing-whitespace-mode 'clean)
             ))

(setq auto-mode-alist
      (append
       '(("\\.h$" . c++-mode))
       '(("\\.[wW][xX][sS]\\'" . xml-mode))
       '(("\\.[mM][aA][kK]\\'" . makefile-gmake-mode))
       '(("[mM][aA][kK][eE][fF][iI][lL][eE]\\'" . makefile-gmake-mode))
       '(("\\.[hg]s$"  . haskell-mode))
       '(("\\.hi$"     . haskell-mode))
       '(("\\.l[hg]s$" . literate-haskell-mode))
       '(("/COMMIT_EDITMSG$" . text-mode))
       '(("/TAG_EDITMSG$" . text-mode))
       '(("\\.org$" . org-mode))
       '(("\\.yaws$" . html-mode))
       '(("\\.php3?$" . html-mode))
       auto-mode-alist))

(setf completion-ignored-extensions
      (append completion-ignored-extensions
              '(".hi" ".pdf" "*.o")))

(when (locate-library "template")
  (require 'template)
  (add-to-list 'template-find-file-commands 'ff-find-other-file)
  (setq template-expansion-alist '(("CPP_HEADER_GUARD" (insert (let ((dir (car (reverse (split-string (car template-file) "/" t)))))
                                                                 (if (/= 0 (length dir))
                                                                     (setq dir (concat dir "_")))
                                                                 (let ((filename (concat dir (nth 2 template-file) (nth 4 template-file))))
                                                                   (do ((index (- (length filename) 1) (- index 1))
                                                                        (result nil)
                                                                        (need-separator nil))
                                                                       ((< index 0) (apply 'string result))
                                                                     (let ((case-fold-search nil))
                                                                       (cond ((string-match-p "[[:upper:]]" (substring filename index (+ index 1)))
                                                                              (push (aref filename index) result)
                                                                              (if (/= 0 index)
                                                                                  (setq need-separator t)))
                                                                             ((char-equal ?. (aref filename index))
                                                                              (push ?_ result)
                                                                              (setq need-separator nil))
                                                                             ((char-equal ?_ (aref filename index))
                                                                              (push ?_ result)
                                                                              (setq need-separator nil))
                                                                             (t
                                                                              (if need-separator
                                                                                  (push ?_ result))
                                                                              (push (upcase (aref filename index)) result)
                                                                              (setq need-separator nil)))))))))))
  (template-initialize))

(when (locate-library "haskell-mode")
  (autoload 'haskell-mode "haskell-mode"
    "Major mode for editing Haskell scripts." t)
  (autoload 'literate-haskell-mode "haskell-mode"
    "Major mode for editing literate Haskell scripts." t)
  (add-hook 'haskell-mode-hook 'turn-on-haskell-decl-scan)
  (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
  (add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
  (setq haskell-font-lock-symbols t)
  (autoload 'inferior-haskell-load-file "inf-haskell"
    "Major mode for Haskell interaction." t))

(setq ispell-dictionary "en_US")
(setq ispell-program-name "aspell")

(add-hook 'text-mode-hook
          '(lambda ()
             (if (buffer-file-name (current-buffer))
                 (if (member (file-name-nondirectory (buffer-file-name (current-buffer))) '("COMMIT_EDITMSG" "TAG_EDITMSG"))
                     (flyspell-mode t))
               )))

(when (locate-library "org-install")
  (require 'org-install)
  (setq org-todo-keywords (quote ((sequence "TODO(t)" "STARTED(s)" "|" "DONE(d!/!)" "CANCELLED(c@/!)"))))

  (setq org-agenda-files '("~/agenda"))

  (add-hook 'org-mode-hook
            '(lambda ()
               (flyspell-mode t)
               ))
  ;;(setq org-log-done 'time)
  (global-set-key "\C-cl" 'org-store-link)
  (global-set-key "\C-ca" 'org-agenda)
  (global-set-key "\C-cb" 'org-iswitchb))

;; Bookmarks
(global-set-key [f2] 'bookmark-jump)
(global-set-key [C-f2] 'bookmark-set)
(global-set-key [M-f2] 'bookmark-bmenu-list)
(setq bookmark-save-flag nil)

(defvar gud-run-args ""
  "Arguments for running program inside GDB")

(defvar gud-run-args-history nil
  "History for arguments for running program inside GDB")

(add-hook 'gdb-mode-hook
          '(lambda ()
            (global-set-key (kbd "<f10>") 'gud-next)
            (global-set-key (kbd "<f11>") 'gud-step)
            (global-set-key (kbd "<f5>") 'gud-cont)
            (global-set-key (kbd "C-<f5>") '(lambda ()
                                             (interactive)
                                             (switch-to-buffer gud-comint-buffer)
                                             (let ((args (if current-prefix-arg
                                                             (read-shell-command "Run with args: " gud-run-args
                                                                                 (if (equal (car gud-run-args-history) gud-run-args)
                                                                                     '(gud-run-args-history . 1)
                                                                                     'gud-run-args-history))
                                                             gud-run-args)))
                                               (unless (equal args gud-run-args)
                                                 (setq gud-run-args args))
                                               (gud-call (concat "run " args)))))
            (global-set-key (kbd "S-<f5>") 'gud-finish)))


(when (locate-library "w3m")
  (require 'w3m-load))

(when (or (locate-library "slime") (locate-library "~/quicklisp/slime-helper"))
  (when (locate-library "~/quicklisp/slime-helper")
    (load "~/quicklisp/slime-helper"))
  (require 'slime)
  (when running-windows
    (add-to-list 'slime-lisp-implementations '(sbcl ("sbcl")))
    (setq slime-default-lisp 'sbcl))
  (when running-mac
    (add-to-list 'slime-lisp-implementations '(ccl ("ccl64" "-K" "utf8")))
    (setq slime-default-lisp 'ccl))
  (slime-setup '(slime-fancy slime-asdf))
  (when (locate-library "closure-template-html-mode")
    (autoload 'closure-template-html-mode "closure-template-html-mode" "" t)
    (add-to-list 'auto-mode-alist
                 '("\\.tmpl$" . closure-template-html-mode)))
  (setq common-lisp-hyperspec-root "file:///home/alexey/docs/lisp/HyperSpec/")
  (when (locate-library "w3m")
    (require 'hyperspec)
    (defun hyperspec-lookup (&optional symbol-name)
      (interactive)
      (let ((browse-url-browser-function 'w3m-browse-url))
        (if symbol-name
            (common-lisp-hyperspec symbol-name)
          (call-interactively 'common-lisp-hyperspec)))))
  (add-hook 'slime-mode-hook
            '(lambda ()
               (local-set-key (kbd "<RET>") 'newline-and-indent)
               (delete-trailing-whitespace-mode 'clean)
               ;;(unless (slime-connected-p)
               ;;  (save-excursion (slime)))))
               )))

(when (locate-library "mof-mode")
  (require 'mof-mode))

(when (locate-library "closure-template-html-mode")
  (autoload 'closure-template-html-mode "closure-template-html-mode" "Major mode for editing Closure Templates" t)
  (add-to-list 'auto-mode-alist '("\\.tmpl\\'" . closure-template-html-mode))
  (add-hook 'closure-template-html-mode-hook
            '(lambda ()
               (define-key closure-template-html-mode-map (kbd "C-c C-l") 'closure-template-compile)
               (setq show-trailing-whitespace t)
               (delete-trailing-whitespace-mode 'clean)
               )))

(when (locate-library "php-mode")
  (autoload 'php-mode "php-mode" "Major mode `php-mode' for editing PHP code." t))

(when (locate-library "uniquify")
  (require 'uniquify)
  (setq uniquify-buffer-name-style 'reverse)
  (setq uniquify-separator "/")
  (setq uniquify-after-kill-buffer-p t)
  (setq uniquify-ignore-buffers-re "^\\*")
  (setq post-forward-angle-brackets 'post-forward-angle-brackets))
