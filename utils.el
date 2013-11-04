(provide 'utils)

(defun list-filter (condp lst)
  "You guessed it, elisp doesn't have a list filter function, what a joke."
  (let ((fn	'(lambda (x) (and (funcall condp x) x))))
    (delq nil (mapcar fn lst))))

(defun alist-put (alst-name key val)
  "This is a disgrace, why elisp doesn't have this by default is beyond me."
  (let ((alst (symbol-value alst-name))
        (cell (cons key val)))
    (set alst-name
          (cons cell alst))))

(defun load-files-in-dir (dir)
  (let ((file-in-dir-p '(lambda (x) (file-regular-p (format "%s/%s" dir x)))))
    (dolist (f (list-filter file-in-dir-p (directory-files dir)))
      (load-file (format "%s/%s" dir f)))))
