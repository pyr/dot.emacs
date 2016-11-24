;;
;; Globally map C-c t to a light/dark theme switcher
;; Also pull-in graphene for better fonts

(custom-set-variables '(solarized-termcolors 256))

(setq solarized-default-background-mode 'dark)
(setq gruvbox-default-background-mode 'dark)

(load-theme 'solarized t)
(load-theme 'gruvbox t)

(defun set-background-mode (frame mode)
  (set-frame-parameter frame 'background-mode mode)
  (when (not (display-graphic-p frame))
    (set-terminal-parameter (frame-terminal frame) 'background-mode mode))
  (enable-theme (if (eq mode 'dark) 'gruvbox 'solarized)))

(defun switch-theme ()
  (interactive)
  (let ((mode  (if (eq (frame-parameter nil 'background-mode) 'dark)
                   'light 'dark)))
    (set-background-mode nil mode)))

(add-hook 'after-make-frame-functions
          (lambda (frame) (set-background-mode frame gruvbox-default-background-mode)))

(set-background-mode nil gruvbox-default-background-mode)

(global-set-key (kbd "C-c t") 'switch-theme)

(add-to-list 'default-frame-alist '(font . "Terminus 12"))
(when (display-graphic-p nil)
  (set-face-attribute 'default t :font "Terminus 12"))
