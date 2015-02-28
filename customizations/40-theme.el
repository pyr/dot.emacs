;;
;; Globally map C-c t to a light/dark theme switcher
;; Also pull-in graphene for better fonts

(custom-set-variables '(solarized-termcolors 256))

(load-theme 'solarized t)

(defun set-background-mode (mode)
  (set-frame-parameter nil 'background-mode mode)
  (when (not (display-graphic-p (selected-frame)))
    (set-terminal-parameter nil 'background-mode mode))
  (enable-theme 'solarized))

(defun switch-theme ()
  (interactive)
  (let ((mode  (if (eq (frame-parameter nil 'background-mode) 'dark)
                   'light 'dark)))
    (set-background-mode mode)))

(set-background-mode 'dark)

(global-set-key (kbd "C-c t") 'switch-theme)

(require 'graphene)
