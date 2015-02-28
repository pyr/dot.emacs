;;
;; Globally map C-c t to a light/dark theme switcher
;; Also pull-in graphene for better fonts
;;(require 'solarized)

(load-theme 'solarized t)

(defun get-frame ()
  (window-frame (frame-first-window)))

(defun switch-theme ()
  (interactive)
  (let* ((bmode (frame-parameter (get-frame) 'background-mode))
         (mode  (if (eq bmode 'light) 'dark 'light)))
    (message (format "switching frame background-mode to: %s"
                     (symbol-name mode)))
    (set-frame-parameter (get-frame)
                         'background-mode
                         mode)
    (enable-theme 'solarized)))

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
