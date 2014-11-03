;; -*- mode: emacs-lisp -*-
(defvar running-windows (eq system-type 'windows-nt))
(defvar running-x (eq window-system 'x))
(defvar running-mac (eq window-system 'ns))

(when running-mac
  (set-input-method 'russian-computer)
  (deactivate-input-method)
  (setenv "PATH" (concat (getenv "PATH") ":/usr/texbin:/usr/local/bin"))
  (setq exec-path (append exec-path '("/usr/texbin" "/usr/local/bin"))))

(when running-windows
    (prefer-coding-system 'windows-1251))
(prefer-coding-system 'utf-8)
;;(set-default-coding-systems 'utf-8)
;;(set-terminal-coding-system 'utf-8)
;;(set-keyboard-coding-system 'utf-8)

(when running-windows
  (set-default-font "DejaVu Sans Mono-11")
  (add-to-list 'default-frame-alist
               '(font . "DejaVu Sans Mono-11")))

(when running-mac
;  (set-default-font "-*-DejaVu_Sans_Mono-medium-normal-*-11-*-*-*-m-0-iso10646-1")
;  (add-to-list 'default-frame-alist
;               '(font . "-*-DejaVu_Sans_Mono-medium-normal-*-11-*-*-*-m-0-iso10646-1"))
;  (set-fontset-font t 'cyrillic (font-spec :name "DejaVu Sans Mono")))
  (set-fontset-font t 'cyrillic (font-spec :name "Monaco")))

(when (file-exists-p "~/.emacs.d/site-lisp")
  (add-to-list 'load-path "~/.emacs.d/site-lisp")
  (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
      (let* ((my-lisp-dir "~/.emacs.d/site-lisp/")
             (default-directory my-lisp-dir))
        (setq load-path (cons my-lisp-dir load-path))
        (normal-top-level-add-subdirs-to-load-path))))

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
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(initial-scratch-message nil)
; '(iswitch-buffer-ignore (quote ("Minibuf-[0-9]+")))
; '(iswitchb-mode t)
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

(fset 'yes-or-no-p 'y-or-n-p)
(setq make-backup-files nil)
(setq split-width-threshold most-positive-fixnum)

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

(require 'compile)
(setq compilation-scroll-output t)
(put 'compile-command 'safe-local-variable 'stringp)

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
             (if (not (save-excursion (goto-char (point-min))
                                      (re-search-forward "[[:blank:]]$" nil t)))
                 (delete-trailing-whitespace-mode 1))
             ))

(add-hook 'c-mode-common-hook
          '(lambda ()
             (local-set-key (kbd "<RET>") 'newline-and-indent)
             (local-set-key (kbd "\C-co") 'ff-find-other-file)
             (local-set-key (kbd "<f7>") 'compile)
             (local-set-key (kbd "C-c M-m") 'my-imenu)
             (local-set-key (kbd "C-c , s") 'semantic-analyze-proto-impl-toggle)
             (modify-syntax-entry ?_ "w")
             (setq show-trailing-whitespace t)
             (when (locate-library "gtags")
               (gtags-mode 1))
             ;(ede-minor-mode 1)
;             (if (not (save-excursion (goto-char (point-min))
;                                      (re-search-forward "[[:blank:]]$" nil t)))
             (delete-trailing-whitespace-mode 'clean)
             ))

(add-hook 'makefile-mode-hook
          '(lambda ()
             (modify-syntax-entry ?_ "w")
             ))

(add-hook 'python-mode-hook
          '(lambda ()
             (define-key python-mode-map (kbd "<RET>") 'newline-and-indent)
             (modify-syntax-entry ?_ "w")
             (setq show-trailing-whitespace t)
             ))

(add-hook 'latex-mode-hook
          '(lambda ()
             (define-key latex-mode-map (kbd "<RET>") 'newline-and-indent)
             (turn-on-auto-fill)
             (setq show-trailing-whitespace t)
             (delete-trailing-whitespace-mode 'clean)
             (local-set-key (kbd "<f7>") 'compile)
             ))

(add-hook 'haskell-mode-hook
          '(lambda ()
             (define-key haskell-mode-map [?\C-c ?\C-r] 'inferior-haskell-reload-file)
             (setq show-trailing-whitespace t)
             (if (not (save-excursion (goto-char (point-min))
                                      (re-search-forward "[[:blank:]]$" nil t)))
                 (delete-trailing-whitespace-mode 1))
             ))

(add-hook 'erlang-mode-hook
          '(lambda ()
             (define-key erlang-mode-map (kbd "<RET>") 'newline-and-indent)
             (setq show-trailing-whitespace t)
             (if (not (save-excursion (goto-char (point-min))
                                      (re-search-forward "[[:blank:]]$" nil t)))
                 (delete-trailing-whitespace-mode 1))
             ))

(add-hook 'html-mode-hook
          '(lambda ()
            (define-key html-mode-map (kbd "<RET>") 'newline-and-indent)
            (setq show-trailing-whitespace t)
            (mmm-mode-on)
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

(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

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

;; (cond ((and
;;        running-windows
;;        (equal (getenv "USERDOMAIN") "TRANSAS"))
;;       (setq tags-table-list
;;             '("C:/VS.NET2003/Vc7/include" "C:/VS.NET2003/Vc7/PlatformSDK/Include"))
;;       (custom-set-variables
;;        '(ps-lpr-command "lpr")
;;        '(ps-lpr-switches (quote ("-S" "cruncher" "-P" "lp" "-o" "l"))))))

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
;  (add-to-list 'slime-lisp-implementations '(sbcl ("sbcl")))
;  (setq slime-default-lisp 'sbcl)
  (add-to-list 'slime-lisp-implementations '(ccl ("/opt/ccl/scripts/ccl64" "-K" "utf8")))
  (setq slime-default-lisp 'ccl)
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
               (if (not (save-excursion (goto-char (point-min))
                                        (re-search-forward "[[:blank:]]$" nil t)))
                   (delete-trailing-whitespace-mode 1))
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
               (if (not (save-excursion (goto-char (point-min))
                                        (re-search-forward "[[:blank:]]$" nil t)))
                   (delete-trailing-whitespace-mode 1)))))

(add-hook 'js-mode-hook
          '(lambda ()
             (setq show-trailing-whitespace t)
             (if (not (save-excursion (goto-char (point-min))
                                      (re-search-forward "[[:blank:]]$" nil t)))
                 (delete-trailing-whitespace-mode 1))))
(add-hook 'js-mode-hook #'fic-mode)

(when (locate-library "php-mode")
  (autoload 'php-mode "php-mode" "Major mode `php-mode' for editing PHP code." t))
  ;(add-to-list 'auto-mode-alist '("\\.php\\'" . php-mode)))

(when (locate-library "uniquify")
  (require 'uniquify)
  (setq uniquify-buffer-name-style 'reverse)
  (setq uniquify-separator "/")
  (setq uniquify-after-kill-buffer-p t)
  (setq uniquify-ignore-buffers-re "^\\*")
  (setq post-forward-angle-brackets 'post-forward-angle-brackets))

(when (locate-library "egg")
  (require 'egg))
