(setq local-current-theme 'solarized-light)

(defun switch-theme-to (name)
  (setq local-current-theme name)
  (load-theme name))

(defun switch-theme ()
  (interactive)
  (pcase local-current-theme
    (`solarized-light (switch-theme-to 'solarized-dark))
    (`solarized-dark  (switch-theme-to 'solarized-light))))

(custom-set-variables
  '(custom-safe-themes 
    (quote 
     ("fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6"
      "1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365"
      default))))

(global-set-key (kbd "C-c t") 'switch-theme)

;;(when window-system
;;  (load-theme 'solarized-light))

(require 'graphene)
