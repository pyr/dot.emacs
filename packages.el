(require 'package)

(defun install (sym) (unless (package-installed-p sym) (package-install sym)))

(setq package-archives
  '(("ELPA"         . "http://tromey.com/elpa/")
    ("gnu"          . "http://elpa.gnu.org/packages/")
    ("melpa"        . "http://melpa.org/packages/")
    ("melpa-stable" . "http://stable.melpa.org/packages/")
    ("org"          . "http://orgmode.org/elpa/")))

(setq package-pinned-packages
  '((cider . "melpa-stable")
    (company . "melpa-stable")
    (projectile . "melpa-stable")
    (gist . "melpa-stable")
    (smex . "melpa-stable")
    (ag . "melpa-stable")
    (paredit . "melpa-stable")
    (smartparens . "melpa-stable")
    (rainbow-delimiters . "melpa-stable")
    (flx-ido . "melpa-stable")
    (flymake-python-pyflakes . "melpa-stable")
    (powerline . "melpa-stable")
    (ox-reveal . "melpa")
    (cquery . "melpa")
    (go-mode . "melpa-stable")
    (rust-mode . "melpa-stable")
    (lua-mode . "melpa-stable")
    (puppet-mode . "melpa-stable")
    (yaml-mode . "melpa-stable")
    (markdown-mode . "melpa")
    (web-mode . "melpa-stable")
    (clojure-mode . "melpa-stable")
    (terraform-mode . "melpa-stable")
    (flycheck-joker . "melpa-stable")
    (gruvbox-theme . "melpa-stable")
    (ghub . "melpa")
    (magit . "melpa")
))

;; XXX: we could test for an out of date refresh file
;; and re-update accordingly
(when (not (file-exists-p "~/.emacs.d/packages-refreshed"))
  (package-refresh-contents)
  (write-region "" "" "~/.emacs.d/packages-refreshed"))

(package-initialize t)

(setq install-list (mapcar 'car package-pinned-packages))
(mapc 'install install-list)
