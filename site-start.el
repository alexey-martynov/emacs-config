(when (file-exists-p "~/.emacs.d/site-lisp")
  (add-to-list 'load-path "~/.emacs.d/site-lisp")
  (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
      (let* ((my-lisp-dir "~/.emacs.d/site-lisp/")
             (default-directory my-lisp-dir))
        (setq load-path (cons my-lisp-dir load-path))
        (normal-top-level-add-subdirs-to-load-path))))

(defun avm-get-configuration-directories ()
  "Build list of directories with customized Lisp modes and other sources"
  (let (dirs)
    (when (file-exists-p "~/.emacs.d/site-lisp")
      (let* ((directory-lister (lambda (path)
                                 (message "Extracting autoloads from %s" path)
                                 (mapc #'(lambda (file)
                                           (when (file-directory-p file)
                                             (setf dirs (cons file dirs))
                                             (funcall directory-lister file)))
                                       (directory-files path t "[^.].*" t)))))
        (funcall directory-lister "~/.emacs.d/site-lisp")
        (cons "~/.emacs.d/site-lisp" dirs)))))

;; This function needs to be here to be
;; accessible from batch invocation
(defun update-all-autoloads ()
  (interactive)
  (let ((generated-autoload-file "~/.emacs.d/site-lisp/loaddefs"))
    (when (not (file-exists-p generated-autoload-file))
      (with-current-buffer (find-file-noselect generated-autoload-file)
        (insert ";;") ;; create the file with non-zero size to appease autoload
        (save-buffer)))
    (let ((dirs (avm-get-configuration-directories)))
      (when dirs
        (apply  #'update-directory-autoloads (cons "~/.emacs.d/site-lisp" dirs))))))

(defun recompile-all-configuration ()
  (mapc #'(lambda (path)
            (message "Compiling files in %s" path)
            (byte-recompile-directory path 0))
        (avm-get-configuration-directories)))
