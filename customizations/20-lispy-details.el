(defun gen-paredit-hook ()
  (paredit-mode +1))

(defun fix-paredit-delete ()
  (local-set-key (kbd "C-h") 'paredit-backward-delete))

(add-hook 'emacs-lisp-mode-hook 'gen-paredit-hook)
(add-hook 'paredit-mode-hook 'fix-paredit-delete)
(add-hook 'clojure-mode-hook 'gen-paredit-hook)
(add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)
(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
(add-hook 'cider-mode-hook 'gen-paredit-hook)
(add-hook 'cider-repl-mode-hook 'gen-paredit-hook)
