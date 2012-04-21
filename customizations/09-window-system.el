(defun set-default-font (frame)
  (when window-system
    (set-face-attribute 'default nil :font "Inconsolata 12")))

(setq default-frame-alist '((font-backend . "xft")
			    (font . "Inconsolata 12")))

(set-default-font nil)

(add-hook 'after-make-frame-functions 'set-default-font)
