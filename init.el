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
      gc-cons-threshold 50000000
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
      shell-file-name "/bin/bash"
      hippie-expand-try-functions-list
      '(try-expand-all-abbrevs try-expand-dabbrev
                               try-expand-dabbrev-all-buffers
                               try-expand-dabbrev-from-kill
                               try-complete-file-name-partially
                               try-complete-file-name
                               try-expand-all-abbrevs
                               try-expand-list
                               try-expand-line
                               try-complete-lisp-symbol-partially
                               try-complete-lisp-symbol))

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
(global-set-key (kbd "C-x r") 'query-replace)
(global-set-key "\C-x\C-r" 'query-replace)
(global-set-key (kbd "C-.") 'find-tag)
(global-set-key (kbd "C-,") 'pop-tag-mark)
(global-set-key (kbd "C-;") 'comment-or-uncomment-region)
(global-set-key (kbd "M-i") 'hippie-expand)

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

;; ui
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))

(setq-default cursor-in-non-selected-windows nil)

(fset 'yes-or-no-p 'y-or-n-p)

;; add the current line number to the mode bar
(line-number-mode t)

;; add the current column number to the mode bar
(column-number-mode t)

;; case insensitive searches
(set-default 'case-fold-search t)

;; typed text replaces the selection if the selection is active
(delete-selection-mode t)

;; make emacs use the clipboard if running in X
(when window-system
  (setq select-enable-clipboard t
        interprogram-paste-function 'x-cut-buffer-or-selection-value))

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

(use-package paren
  :pin "melpa-stable"
  :config
  (show-paren-mode +1))

(use-package elec-pair
  :pin "melpa-stable"
  :config
  (electric-pair-mode +1))

;; uniquify buffer names: append path if buffer names are identical
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

(use-package idle-highlight-mode
  :pin "melpa-stable"
  :ensure t)

(use-package smex
  :pin "melpa-stable"
  :ensure t)

(use-package diminish
  :pin "melpa-stable"
  :ensure t
  :demand t)

;; internal
(use-package whitespace
  :diminish
  :init
  (dolist (hook '(prog-mode-hook text-mode-hook))
    (add-hook hook #'whitespace-mode))
  (add-hook 'before-save-hook 'delete-trailing-whitespace)
  :config
  (setq whitespace-style '(face trailing lines-tail)
        whitespace-global-modes '(not erc-mode)
        whitespace-line-column 80))

(use-package hl-todo
  :pin "melpa-stable"
  :ensure t
  :config
  (setq hl-todo-highlight-punctuation ":")
  (global-hl-todo-mode))

;; internal
(use-package hl-line
  :config
  (global-hl-line-mode +1))

;; internal
(use-package dired
  :config
  ;; dired - reuse current buffer by pressing 'a'
  (put 'dired-find-alternate-file 'disabled nil)

  ;; always delete and copy recursively
  (setq dired-recursive-deletes 'always)
  (setq dired-recursive-copies 'always)

  ;; if there is a dired buffer displayed in the next window, use its
  ;; current subdir, instead of the current subdir of this dired buffer
  (setq dired-dwim-target t)

  ;; enable some really cool extensions like C-x C-j(dired-jump)
  (require 'dired-x))

(use-package magit
  :pin "melpa-stable"
  :ensure t
  :bind (("C-x g" . magit-status)
         ("C-c C-g" . magit-status)))

(use-package company-quickhelp
  :pin "melpa-stable"
  :ensure t)

(use-package company
  :pin "melpa-stable"
  :after cider
  :ensure t
  :init
  (setq company-tooltip-align-annotations t
        company-minimum-prefix-length 1
        company-require-match nil
        company-idle-delay 0.3
        company-tooltip-limit 10
        company-frontends '(company-pseudo-tooltip-unless-just-one-frontend
	                        company-preview-frontend
	                        company-echo-metadata-frontend))
  :config
  (company-quickhelp-mode 1)
  (global-company-mode)
  :bind
  (:map company-active-map
        ("M-n" . nil)
        ("M-p" . nil)
        ("C-n" . company-select-next)
        ("C-p" . company-select-previous)
        ("C-d" . company-show-doc-buffer)
        ("<tab>" . company-complete-selection)))

(use-package expand-region
  :pin "melpa-stable"
  :ensure t
  :bind (("C-o" . er/expand-region)
         ("C-M-o" . er/contract-region)))

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
  (add-hook 'erlang-mode-hook #'paredit-mode)
  :config
  (defun my-paredit-delete ()
    "If a region is active check if it is balanced and delete it otherwise
        fallback to regular paredit behavior"
    (interactive)
    (if mark-active
        (paredit-delete-region (region-beginning) (region-end))
      (paredit-backward-delete)))
  (define-key paredit-mode-map (kbd "C-j") nil)
  :bind (:map paredit-mode-map
              ("C-M-h" . paredit-backward-kill-word)
              ("C-h" . my-paredit-delete)
              ("<delete>" . my-paredit-delete)
              ("DEL" . my-paredit-delete)))

(use-package clojure-mode
  :pin "melpa-stable"
  :ensure t
  :after flycheck-clj-kondo
  :config
  (require 'flycheck-clj-kondo)
  (add-hook 'clojure-mode-hook #'paredit-mode))

(use-package cider
  :pin "melpa-stable"
  :ensure t
  :config
  (setq nrepl-log-messages t)
  (add-hook 'cider-repl-mode-hook #'paredit-mode)
  (add-hook 'cider-mode-hook #'eldoc-mode)
  (add-hook 'cider-repl-mode-hook #'eldoc-mode))

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

;; internal
(use-package js-mode
  :defer t
  :mode ("\\.json$" . js-mode)
  :config
  (add-hook 'js-mode-hook 'yas-minor-mode))

(use-package nginx-mode
  :pin "melpa-stable"
  :ensure t)

(use-package doom-modeline
  :pin "melpa-stable"
  :ensure t
  :config
  (setq doom-modeline-buffer-encoding nil)
  :hook (after-init . doom-modeline-mode))

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

;; internal
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

(use-package adoc-mode
  :ensure t
  :mode "\\.adoc\\'")

(use-package gist
  :pin "melpa-stable"
  :ensure t)

(use-package plantuml-mode
  :pin "melpa-stable"
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.plantuml\\'" . plantuml-mode)))

(use-package twilight-bright-theme
  :pin "melpa"
  :disabled t
  :ensure t)

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

(use-package flycheck-dialyzer
  :ensure t)

(use-package flycheck-pos-tip
  :ensure t)

(use-package flycheck
  :ensure t
  :config
  (flycheck-pos-tip-mode)
  (add-hook 'after-init-hook #'global-flycheck-mode))

(use-package flycheck-clj-kondo
  :ensure t)

(use-package flyspell
  :ensure t
  :config
  (setq ispell-program-name "aspell" ; use aspell instead of ispell
        ispell-extra-args '("--sug-mode=ultra"))
  (add-hook 'text-mode-hook #'flyspell-mode)
  (add-hook 'prog-mode-hook #'flyspell-prog-mode)
  :bind (:map flyspell-mode-map
              ("C-;" . comment-or-uncomment-region)))

(use-package docker
  :ensure t)

(use-package dockerfile-mode
  :ensure t)

(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))

(use-package clojure-snippets
  :ensure t)

(use-package ripgrep
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

(use-package lsp-mode
  :ensure t
  :pin "melpa-stable"
  :commands (lsp lsp-deferred)
  :hook (go-mode . lsp-deferred))

;; config changes made through the customize UI will be stored here
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(when (file-exists-p custom-file)
  (load custom-file))

(use-package ag
  :ensure t)

(use-package ox-reveal
   :ensure t)

(require 'ansi-color)

(defun display-ansi-colors ()
  "Display ANSI colors in buffer"
  (interactive)
  (ansi-color-apply-on-region (point-min) (point-max)))

(use-package groovy-mode
  :ensure t)

(use-package lua-mode
  :ensure t)

(use-package rust-mode
  :ensure t)

(use-package protobuf-mode
  :ensure t)

(use-package vscode-dark-plus-theme
  :ensure t
  :init (load-theme 'vscode-dark-plus t))

(use-package ctrlf
  :ensure t
  :pin "melpa-stable"
  :init (ctrlf-mode +1))

;; (use-package project
;;   :init (setq project-ignores ".clj-kondo")
;;   :bind (("C-x f" . project-find-file)))

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
          (lambda () (projectile-project-root)))
)

(use-package consult-flycheck
  :after consult
  :config
  (setq flycheck-display-errors-delay 0.5)
  :bind (("C-x C-l" . consult-flycheck)
         ("C-x l" . consult-flycheck)))

(use-package marginalia
  :ensure t
  :config
  (marginalia-mode))

(use-package embark
  :ensure t
  :bind
  (("C-S-a" . embark-act)) ;; alternative for `describe-bindings'

  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure t
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; Enable vertico
(use-package vertico
  :ensure t
  :init
  (vertico-mode)

  ;; Optionally enable cycling for `vertico-next', `vertico-previous',
  ;; `vertico-next-group' and `vertico-previous-group'.
  ;; (setq vertico-cycle t)
  )
(use-package orderless
  :ensure t
  :init
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))


;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :ensure t
  :pin "melpa-stable"
  :init
  (savehist-mode))

;; A few more useful configurations...
(use-package emacs
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  (defun crm-indicator (args)
    (cons (concat "[CRM] " (car args)) (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Grow and shrink minibuffer
  ;;(setq resize-mini-windows t)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Allow usage of set goal column
  (put 'set-goal-column 'disabled nil)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t))

(use-package consult-projectile
  :load-path "~/.emacs.d/elisp/consult-projectile")

(provide 'init)
;;; init.el ends here
