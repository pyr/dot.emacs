;(defun set-default-font (frame)
;  (when window-system
;    (cond
;     ((string-match "apple-darwin" system-configuration)
;      (set-face-attribute 'default nil :font "Inconsolata 12"))
;     ((string-match "linux" system-configuration)
 ;     (set-face-attribute 'default nil :font "Terminus 10")))))

;(cond
; ((string-match "apple-darwin" system-configuration)
;  (setq default-frame-alist '((font-backend . "xft")
;			      (font . "Inconsolata 12"))))
; ((string-match "linux" system-configuration)
;  (setq default-frame-alist '((font-backend . "xft")
;			      (font . "Terminus 10")))))

;(set-default-font nil)
;(add-hook 'after-make-frame-functions 'set-default-font)
