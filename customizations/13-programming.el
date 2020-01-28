;;
;; Programming language specific tweaks
;; ====================================
;;

(require 'company)
(require 'projectile)
(require 'smartparens)
(require 'magit)
(require 'gist)
(require 'ag)
(require 'cider)
(require 'cider-apropos)
(require 'flycheck-clj-kondo)
(require 'flymake-python-pyflakes)
(require 'puppet-mode)
(require 'markdown-mode)
(require 'yaml-mode)
(require 'web-mode)
(require 'go-mode)
(require 'cquery)

;; Generic
;; =======

(projectile-global-mode)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

;; C and Java modes
;; ================
(defun prepare-c-indent ()
  (setq c-basic-offset 4
	tab-width 4
	indent-tabs-mode nil))
(add-hook 'c-mode-common-hook 'prepare-c-indent)

(setq c-default-style '(("c-mode" . "k&r")))

;; Java Mode
;; =========

;; Fix @Override indentation in Java
(add-hook 'java-mode-hook
          '(lambda ()
             "Treat Java 1.5 @-style annotations as comments."
             (setq c-comment-start-regexp
                   "\\(@\\|/\\(/\\|[*][*]?\\)\\)")
             (modify-syntax-entry ?@ "< b" java-mode-syntax-table)))

(add-hook 'java-mode-hook 'prepare-c-indent)

(add-hook 'groovy-mode-hook
	  (lambda ()
	    (setq c-basic-offset 2
		  tab-width 2
		  groovy-indent-offset 2
		  indent-tabs-mode nil)))

;; Python Mode
;; ===========

;; inline notification of python lines violating PEP8
(add-hook 'python-mode-hook 'flymake-python-pyflakes-load)
(setq flymake-python-pyflakes-executable "flake8")

;; Clojure & Lisp
;; ==============

(defun turn-on-delete-trailing-whitespace ()
  (add-hook 'before-save-hook #'delete-trailing-whitespace nil t))
(defun turn-on-company-mode () (company-mode t))
(defun turn-on-idle-highlight-mode () (idle-highlight-mode +1))

(defun prepare-smartparens ()
  (turn-on-smartparens-strict-mode)
  (sp-use-paredit-bindings)
  (sp-with-modes sp--lisp-modes
    ;; disable ', it's the quote character!
    (sp-local-pair "'" nil :actions nil)
    ;; also only use the pseudo-quote inside strings where it serve as
    ;; hyperlink.
    (sp-local-pair "`" "'" :when '(sp-in-string-p))))

(add-hook 'cider-mode-hook 'turn-on-eldoc-mode)

(add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)
(add-hook 'css-mode-hook #'aggressive-indent-mode)
(add-hook 'clojure-mode-hook #'aggressive-indent-mode)

(add-hook 'emacs-lisp-mode-hook 'turn-on-idle-highlight-mode)
(add-hook 'clojure-mode-hook 'turn-on-idle-highlight-mode)
(add-hook 'cider-mode-hook 'turn-on-eldoc-mode)

(add-hook 'prog-mode-hook 'turn-on-delete-trailing-whitespace)
(add-hook 'prog-mode-hook 'turn-on-company-mode)
(add-hook 'prog-mode-hook 'prepare-smartparens)

(global-prettify-symbols-mode 1)

;; Tex Mode
;; ========

(eval-after-load "tex"
  '(setcdr (assoc "LaTeX" TeX-command-list)
          '("%`%l%(mode) -shell-escape%' %t"
          TeX-run-TeX nil (latex-mode doctex-mode) :help "Run LaTeX")))


;; Shell Mode
;; ==========

(setq shell-file-name "/bin/bash")
(add-to-list 'process-environment "SHELL=/bin/bash")

;; Ruby Mode
;; =========

;; Some common ruby file names
(dolist
    (regex
     '("\\.watchr$" "\\.arb$" "\\.rake$" "\\.gemspec$" "\\.ru$" "Rakefile$"
       "Gemfile$" "Capfile$" "Guardfile$" "Rakefile$" "Cheffile$" "Vagrantfile$"
       "Berksfile$" "\\.builder$"))
  (add-to-list 'auto-mode-alist `(,regex . ruby-mode)))

;; JSON Mode
;; =========

(push '("\\.json\\'" . json-mode) auto-mode-alist)

;; Plain modes with no further configuration
;; =========================================
