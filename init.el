
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(load-file "~/.emacs.d/packages.el")
(mapc 'load (directory-files "~/.emacs.d/customizations" t "^[0-9]+.*\.el$"))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("8a6f10b3269c42c351776ee32c7ee755472dee4ecdf2177dcbf64d5e02d1ab64" "90ea0413ad77ae560f150de2e3888da4a2088590209705306156a307899a37ec" "ce0788113995714fd96970417e8e71d5182d02bc40cc7ffef307f5e01e55942f" "11878ac0a75f25057b217f74d9098adbd893dc1374da94d81f715b70f4efa00b" "3fd0fda6c3842e59f3a307d01f105cce74e1981c6670bb17588557b4cebfe1a7" "4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" "4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" "8db4b03b9ae654d4a57804286eb3e332725c84d7cdab38463cb6b97d5762ad26" "31a01668c84d03862a970c471edbd377b2430868eccf5e8a9aec6831f1a0908d" "1297a022df4228b81bc0436230f211bad168a117282c20ddcba2db8c6a200743" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "ebd976c3d91de7c6858db3ba07c52238d545a106c8fbf05192913f6d42421621" "fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" "1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365" default)))
 '(package-selected-packages
   (quote
    (htmlize terraform-mode clojure-mode mu4e-multi ox-reveal powerline yaml-mode web-mode smex smartparens rust-mode rainbow-delimiters puppet-mode projectile paredit markdown-mode magit lua-mode gruvbox-theme go-mode gist flymake-python-pyflakes flx-ido company cider ag)))
 '(solarized-termcolors 256))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
