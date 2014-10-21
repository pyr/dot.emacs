(when (eq system-type 'gnu/linux)
  (setq browse-url-browser-function 'browse-url-generic
        browse-url-generic-program "chromium"))
