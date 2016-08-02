;;
;; smex is a much better M-x replacement with history and fuzzy search

(require 'smex)
(setq smex-save-file (concat user-emacs-directory ".smex-items"))
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
