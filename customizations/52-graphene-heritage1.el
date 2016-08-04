;; See https://github.com/rdallasgray/graphene/blob/master/graphene-helper-functions.el

;; Smoother scrolling
;; ==================

(defun --scroll-up-by (n)   (lambda () (interactive) (scroll-up-command n)))
(defun --scroll-down-by (n) (lambda () (interactive) (scroll-down-command n)))


(unless (and (boundp 'mac-mouse-wheel-smooth-scroll)
	     mac-mouse-wheel-smooth-scroll)
  (global-set-key [wheel-down]        (--scroll-up-by 1))
  (global-set-key [wheel-up]          (--scroll-down-by 1))
  (global-set-key [double-wheel-down] (--scroll-up-by 2))
  (global-set-key [double-wheel-up]   (--scroll-down-by 2))
  (global-set-key [triple-wheel-down] (--scroll-up-by 4))
  (global-set-key [triple-wheel-up]   (--scroll-down-by 4)))

;; Save backup files in the temporary directory
(setq backup-directory-alist         `((".*" . ,temporary-file-directory))
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

;; Automatically update buffers when files change
(global-auto-revert-mode t)

;; Enable 'power user' features
(put 'dired-find-alternate-file 'disabled nil)
(put 'upcase-region             'disabled nil)
(put 'downcase-region           'disabled nil)
(put 'narrow-to-region          'disabled nil)
