(provide 'modules)

(setq modules-load-path "~/.emacs.d/vendor/")

(setq el-get-sources
      '((:name gist :type elpa)
	(:name paredit
	       :after (lambda ()
			(progn
				(defun generic-paredit-hook ()
				    (message "loading paredit")
				    (paredit-mode +1)))))
	(:name clojure-mode
	       :after (lambda ()
			(progn
  			  (alist-put 'auto-mode-alist "\\.clj" 'clojure-mode)
  			  (alist-put 'auto-mode-alist "\\.cljs" 'clojure-mode)
    			  (add-hook 'clojure-mode-hook 'generic-paredit-hook)
			  (add-hook 'clojure-mode-hook
				    (lambda ()
				      (progn
				  	(define-key clojure-mode-map "{" 'paredit-open-brace)
    			                (define-key clojure-mode-map "}" 'paredit-close-brace)))))))
	(:name slime
	       :type elpa
	       :after (lambda ()
			(progn
		  	  (add-hook 'slime-mode-hook 'generic-paredit-hook)	
		  	  (setq slime-protocol-version 'ignore))))
	(:name slime-repl
	       :type elpa
	       :after (lambda ()
	  	  	(add-hook 'slime-repl-mode-hook 'paredit-mode-enable)))
	(:name swank-clojure)
	(:name htmlize :type elpa)
	(:name ecb :type elpa)
	(:name markdown-mode
	       :type elpa
	       :after (lambda ()
			(progn
  			  (alist-put 'auto-mode-alist "\\.md" 'markdown-mode)
  			  (alist-put 'auto-mode-alist "\\.markdown" 'markdown-mode))))
	(:name scala-mode :type elpa
	       :after (lambda ()
		        (alist-put 'auto-mode-alist "\\.scala" 'scala-mode)))
	(:name o-blog
	       :type git
	       :url "https://github.com/renard/o-blog")
	(:name zenburn-theme
	       :type elpa
	       :after (lambda () (load-theme 'zenburn t)))
	(:name magit
	       :type elpa)))

(defun sync-packages ()
  (interactive)
  (el-get 'sync '(el-get package))
  (setq local:packages (mapcar 'el-get-source-name el-get-sources))
  (add-to-list 'package-archives '("tromey"      . "http://tromey.com/elpa/")) 
  (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
  (el-get 'sync local:packages)) 

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(if (require 'el-get nil t)
    (url-retrieve
      "https://github.com/dimitri/el-get/raw/master/el-get-install.el"
      (lambda (s)
	(end-of-buffer)
	(eval-print-last-sexp)
	(sync-packages))))
