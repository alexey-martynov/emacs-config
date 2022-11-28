(require 'compile)

(setq compilation-scroll-output t)

;;; Allow COM:PILE-COMMAND to be specified inside files/dir-locals
(put 'compile-command 'safe-local-variable 'stringp)

;;; Enforce GNU Make to show "Entering/Leaving directory"
(setenv "MAKEFLAGS" "-w")

;;; Parse Boost.Test errors
(add-to-list 'compilation-error-regexp-alist '("^\\(.+\\)(\\([[:digit:]]+\\)):[[:space:]]*\\(?:fatal \\)?error" 1 2 nil 2))

;;; Add per-file compilation target as a safe variable
;;; TODO: The following code should be contributed back
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

(defun avm-compilation-mode-keys-hook ()
  (local-set-key (kbd "<f7>") 'recompile))

(add-hook 'compilation-mode-hook #'avm-compilation-mode-keys-hook)

(when (<= 28 emacs-major-version)
  (require 'ansi-color)
  (add-hook 'compilation-filter-hook 'ansi-color-compilation-filter))
