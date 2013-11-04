(defun html-wrap-to-tag ()
  (interactive)
  (let (b e r)
    (save-excursion
      (setq e (point))
      (skip-chars-backward "-_A-Za-z0-9")
      (setq b (point))
      (setq r (buffer-substring b e))
      (kill-region b e))
    (insert (format "<%s></%s>" r r))
    (skip-chars-backward "-_A-Za-z0-9>/")
    (skip-chars-backward "<")))

(add-hook 'html-mode-hook
	  (lambda () (local-set-key (kbd "C-c C-t") 'html-wrap-to-tag)))
