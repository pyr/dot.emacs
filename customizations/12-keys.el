;;
;; Insert global keyboard mappings here
;; The C-h might be controversial since it usually maps to
;; emacs help, feel free to change
(global-set-key (kbd "C-c l") 'goto-line)
(global-set-key (kbd "C-h")   'backward-delete-char)

(require 'smex)
(setq smex-save-file (concat user-emacs-directory ".smex-items"))
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

(defun kill-default-buffer ()
  (interactive)
  (let (kill-buffer-query-functions) (kill-buffer)))

(global-set-key (kbd "C-x k") 'kill-default-buffer)

;; Use ido for general completion
(require 'flx-ido)
(ido-mode 1)
(ido-everywhere 1)

(put 'ido-complete 'disabled nil)
(put 'ido-exit-minibuffer 'disabled nil)
(setq ido-enable-flex-matching t
      ido-auto-merge-work-directories-length nil
      ido-create-new-buffer 'always
      ido-use-faces nil
      ido-use-filename-at-point 'guess)


;; Make buffer names unique
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

(defun local-suspend-frame ()
  "In a GUI environment, do nothing; otherwise `suspend-frame'."
  (interactive)
  (if (display-graphic-p)
      (message "suspend-frame disabled for graphical displays.")
    (suspend-frame)))

(global-unset-key (kbd "C-z"))
(global-set-key (kbd "C-z") 'local-suspend-frame)
