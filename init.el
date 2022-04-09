;;; init.el --- Single file emacs config
;;
;; Author: Max Penet <m@qbits.cc>
;; URL: https://github.com/pyr/dot.emacs
;; Keywords: emacs config

;;; Commentary:

;; Just my Emacs config

;;; License:

;; Copyright (C) 2019  Max Penet
;; Some additional bits by Pierre-Yves Ritschard

;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see
;; <https://www.gnu.org/licenses/>.

;;; Code:


(setq load-prefer-newer t
      use-short-answers t
      gc-cons-threshold (* 100 1024 1024)
      read-process-output-max (* 1024 1024)
      auto-window-vscroll nil
      large-file-warning-threshold 100000000
      browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "xdg-open"
      truncate-partial-width-windows nil
      backup-inhibited t
      make-backup-files nil
      auto-save-default nil
      auto-save-list-file-prefix nil
      vc-follow-symlinks nil
      inhibit-startup-message t
      initial-scratch-message nil
      visible-bell t
      shell-file-name "/bin/bash")

(add-to-list 'process-environment "SHELL=/bin/bash")
(add-to-list 'load-path (expand-file-name "~/.emacs.d/elisp"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GLOBAL BINDINGS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key (kbd "C-h") 'backward-delete-char)
(global-set-key (kbd "C-M-h") 'backward-kill-word)
(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "<C-return>") 'newline)
(global-set-key "\C-x\C-o" 'other-window)
(global-set-key "\C-x\C-k" 'kill-buffer)
(global-set-key (kbd "C-x '") 'delete-other-windows)
(global-set-key (kbd "C-x ,") 'split-window-below)
(global-set-key (kbd "C-x .") 'split-window-right)
(global-set-key (kbd "C-x l") 'delete-window)
(global-set-key (kbd "C-x r") 'query-replace)
(global-set-key "\C-x\C-r" 'query-replace)
(global-set-key (kbd "C-;") 'comment-or-uncomment-region)

(defun local-suspend-frame ()
  "In a GUI environment, do nothing; otherwise `suspend-frame'."
  (interactive)
  (if (display-graphic-p)
      (message "suspend-frame disabled for graphical displays.")
    (suspend-frame)))

(global-unset-key (kbd "C-z"))
(global-set-key (kbd "C-z") 'local-suspend-frame)

(defun kill-default-buffer ()
  "A function used to replace `kill-buffer`.
The most active buffer is selected and killed."
  (interactive)
  (let (kill-buffer-query-functions) (kill-buffer)))

(global-set-key (kbd "C-x k") 'kill-default-buffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LOOK & FEEL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'default-frame-alist '(font . "JetBrains Mono 10"))

;; utf8 only
(setq current-language-environment "UTF-8")
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
;; TAB => 4*'\b'
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default c-basic-offset tab-width)
(setq-default sgml-basic-offset tab-width)
;; UI
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(setq-default cursor-in-non-selected-windows nil)

;; Add the current line number to the mode bar
(line-number-mode t)
;; Add the current column number to the mode bar
(column-number-mode t)
;; Case insensitive searches
(set-default 'case-fold-search t)
;; Typed text replaces the selection if the selection is active
(delete-selection-mode t)
;; Make emacs use the clipboard if running in X
(when window-system
  (setq select-enable-clipboard t
        interprogram-paste-function 'x-cut-buffer-or-selection-value))
;; Config changes made through the customize UI will be stored here
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PACKAGES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'package)

(setq package-archives
      '(("ELPA" . "https://tromey.com/elpa/")
        ("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")
        ("org" . "https://orgmode.org/elpa/"))
      package-user-dir (expand-file-name "elpa" user-emacs-directory)
      package-enable-at-startup nil)

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)

(setq use-package-verbose t)

;; UX

(use-package paren
  :pin "melpa-stable"
  :config
  (show-paren-mode +1))

(use-package uniquify
  :pin "melpa-stable"
  :init (setq uniquify-buffer-name-style 'post-forward-angle-brackets))

(use-package display-line-numbers
  :pin "melpa-stable"
  :if (version<= "26" emacs-version)
  :hook ((prog-mode conf-mode) . display-line-numbers-mode)
  :custom (display-line-numbers-width 3))

(use-package which-function-mode
  :pin "melpa-stable"
  :defer t
  :hook ((prog-mode . which-function-mode)))

(use-package aggressive-indent
  :pin "melpa-stable"
  :ensure t)

(use-package idle-highlight-in-visible-buffers-mode
  :pin "melpa-stable"
  :ensure t)

(use-package doom-modeline
  :pin "melpa-stable"
  :ensure t
  :config
  (setq doom-modeline-buffer-encoding nil)
  :hook (after-init . doom-modeline-mode))

(use-package savehist
  :ensure t
  :pin "melpa-stable"
  :init
  (savehist-mode))

(use-package smex
  :pin "melpa-stable"
  :ensure t)

(use-package diminish
  :pin "melpa-stable"
  :ensure t
  :demand t)

(use-package whitespace
  :diminish t
  :init
  :hook
  ((prog-mode . delete-trailing-whitespace)
   (text-mode . delete-trailing-whitespace))
  :config
  (setq whitespace-style '(face trailing lines-tail)
        whitespace-global-modes '(not erc-mode)
        whitespace-line-column 120))

(use-package emojify
  :ensure t
  :config
  (setq emojify-display-style 'image)
  ;; only replace unicode and github, no ascii)
  (setq emojify-emoji-styles '(unicode github))
  ;; echo the actual underlying character to the minibuffer when point
  ;; is over them so we don't mess with thex displayed buffer itself
  (setq emojify-point-entered-behaviour 'echo)
  (global-emojify-mode 1))

(use-package hl-todo
  :pin "melpa-stable"
  :diminish t
  :ensure t
  :config
  (setq hl-todo-highlight-punctuation ":")
  (global-hl-todo-mode))

(use-package hl-line
  :diminish t
  :config
  (global-hl-line-mode +1))

(use-package all-the-icons
  :pin "melpa-stable"
  :ensure t
  :config (setq all-the-icons-scale-factor 1.0))

(use-package all-the-icons-ivy
  :pin "melpa-stable"
  :ensure t
  :hook (after-init . all-the-icons-ivy-setup))

(use-package rainbow-mode
  :defer t
  :ensure t)

(use-package ctrlf
  :ensure t
  :pin "melpa-stable"
  :init (ctrlf-mode +1))

(use-package marginalia
  :ensure t
  :config
  (marginalia-mode))

(use-package vertico
  :ensure t
  :init
  (vertico-mode))

(use-package orderless
  :ensure t
  :init
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

;; Clojure
(use-package flycheck
  :pin "melpa-stable"
  :ensure t
  :hook
  (after-init . global-flycheck-mode))

(use-package clojure-mode
  :pin "melpa-stable"
  :ensure t)

(use-package vscode-dark-plus-theme
  :pin "melpa-stable"
  :ensure t
  :init (load-theme 'vscode-dark-plus t))

(use-package lsp-mode
  :pin "melpa-stable"
  :commands lsp
  :ensure t
  :config
  (add-to-list 'lsp-language-id-configuration '(clojure-mode       . "clojure-mode"))
  (add-to-list 'lsp-language-id-configuration '(clojurec-mode      . "clojurec-mode"))
  (add-to-list 'lsp-language-id-configuration '(clojurescript-mode . "clojurescript-mode"))
  (setq lsp-keep-workspace-alive nil)
  :hook ((clojure-mode       . lsp)
         (clojurec-mode      . lsp)
         (clojurescript-mode . lsp)
         (go-mode            . lsp)
         (before-safe        . lsp-format-buffer)))


(use-package paredit
  :pin "melpa-stable"
  :ensure t
  :init
  (add-hook 'emacs-lisp-mode-hook #'paredit-mode)
  (add-hook 'lisp-interaction-mode-hook #'paredit-mode)
  (add-hook 'clojure-mode-hook #'paredit-mode)
  (add-hook 'cider-mode-hook #'paredit-mode)
  (add-hook 'cider-repl-mode-hook #'paredit-mode)
  (add-hook 'emacs-lisp-mode-hook #'paredit-mode)
  :config
  :bind (:map paredit-mode-map
              ("C-M-h" . paredit-backward-kill-word)))

(use-package cider
  :ensure t
  :config
  (setq nrepl-log-messages t
        cider-font-lock-dynamically nil ; use lsp semantic tokens
        cider-eldoc-display-for-symbol-at-point nil ; use lsp
        cider-prompt-for-symbol nil)
  :hook ((cider-repl-mode . paredit-mode)
         (cider-mode . (lambda () (remove-hook 'completion-at-point-functions
                                               #'cider-complete-at-point)))))

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode)

;; Other languages

(use-package go-guru
  :pin "melpa-stable"
  :ensure t
  :defer t)

(use-package go-mode
  :ensure t
  :pin "melpa-stable"
  :config
  (add-hook 'go-mode-hook (lambda ()
                            (flycheck-mode)
                            (set (make-local-variable 'company-backends)
                                 '(company-go))))

  (add-hook 'before-save-hook 'gofmt-before-save))

(use-package go-eldoc
  :pin "melpa-stable"
  :ensure t
  :defer t
  :init
  (add-hook 'go-mode-hook 'go-eldoc-setup))

(use-package nginx-mode
  :pin "melpa-stable"
  :ensure t)

(use-package css-mode
  :ensure t
  :config
  (progn (add-hook 'css-mode-hook 'rainbow-mode)
         (setq css-indent-offset tab-width)))

(use-package org
  :defer t)

(use-package htmlize
  :pin "melpa-stable"
  :ensure t)

(use-package markdown-mode
  :pin "melpa-stable"
  :ensure t
  :mode (("\\.md\\'" . gfm-mode)
         ("\\.markdown\\'" . gfm-mode))
  :config
  (setq markdown-fontify-code-blocks-natively t))

(use-package yaml-mode
  :pin "melpa-stable"
  :defer t
  :ensure t)

;; ;; internal
(use-package js-mode
  :defer t
  :mode ("\\.json$" . js-mode)
  :config
  (add-hook 'js-mode-hook 'yas-minor-mode))


(use-package gist
  :pin "melpa-stable"
  :ensure t)

(use-package plantuml-mode
  :pin "melpa-stable"
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.plantuml\\'" . plantuml-mode)))


;; Project stuff

(use-package company
  :ensure t)

(use-package company-quickhelp
  :pin "melpa-stable"
  :after company
  :ensure t)

(use-package ripgrep
  :pin "melpa-stable"
  :ensure t)

(use-package projectile
 :pin "melpa-stable"
 :ensure t
 :init
 (projectile-mode +1)
 (define-key projectile-mode-map
   (kbd "C-c p") 'projectile-command-map)
 (setq projectile-project-search-path '("~/Code/")
       projectile-completion-system 'default)
 :config
 (projectile-mode +1)
 (add-to-list 'projectile-globally-ignored-files ".clj-kondo/*")
 :bind (("C-x f" . projectile-find-file)))

;; Consult

(use-package consult
  :ensure t
  :pin "melpa-stable"
  :bind (;; C-c bindings (mode-specific-map)
         ("C-c h" . consult-history)
         ("C-c m" . consult-mode-command)
         ("C-c b" . consult-bookmark)
         ("C-c k" . consult-kmacro)
         ;; C-x bindings (ctl-x-map)
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ("<help> a" . consult-apropos)            ;; orig. apropos-command
         ;; M-g bindings (goto-map)
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flycheck)
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-project-imenu)
         ;; M-s bindings (search-map)
         ("C-c f f" . consult-find)
         ("C-c f L" . consult-locate)
         ("C-c f g" . consult-grep)
         ("C-c f G" . consult-git-grep)
         ("C-c f r" . consult-ripgrep)
         ("C-c f l" . consult-line)
         ("C-c f m" . consult-multi-occur)
         ("C-c f k" . consult-keep-lines)
         ("C-c f u" . consult-focus-lines)
         ;; Isearch integration
         ("C-c f e" . consult-isearch)
         :map isearch-mode-map
         ("M-e" . consult-isearch)                 ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch)               ;; orig. isearch-edit-string
         ("M-s l" . consult-line))
    :config
    ;; Optionally configure a function which returns the project root directory
    (setq consult-project-root-function
          (lambda () (projectile-project-root))))

(use-package consult-projectile
  :ensure t
  :after (projectile consult))

;; Treemacs

(use-package treemacs
  :ensure t)

(use-package lsp-treemacs
  :ensure t
  :config
  (setq lsp-treemacs-error-list-current-project-only t))

(use-package treemacs-projectile
  :ensure t)

(provide 'init)
;;; init.el ends here
