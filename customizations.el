;;

(setq customizations-load-path "~/.emacs.d/customizations/")

(dolist (f (directory-files customizations-load-path))
  (when (file-regular-p f)
    (message (format "loading %s" f))
    (load-file (format "%s/%s" customizations-load-path f))))
