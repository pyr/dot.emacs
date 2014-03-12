;;
;; always kill trailing whitespace when saving
;;
(add-hook 'prog-mode-hook
 (lambda ()
  (add-hook 'before-save-hook #'delete-trailing-whitespace nil t)))
