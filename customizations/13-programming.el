;;
;; Programming language specific tweaks
;; ====================================
;;

;; Generic
;; =======

(require 'company)
(require 'projectile)
(require 'smartparens)
(require 'magit)
(require 'gist)
(require 'ag)

(projectile-global-mode)
(company-mode t)

;; Always delete trailing whitespace
(add-hook 'prog-mode-hook
	  (lambda ()
	    (add-hook 'before-save-hook #'delete-trailing-whitespace nil t)))

(add-hook 'prog-mode-hook
          (lambda ()
            (company-mode t)
            (smartparens-mode t)
            (sp-with-modes sp--lisp-modes
              (sp-local-pair "'" nil :actions nil)
              (sp-local-pair "`" nil :actions nil))))

(eval-after-load 'flycheck
  '(progn
     (defun --flycheck-display-errors-fn (errors)
       (mapc (lambda (err)
               (message "flyc: %s" (flycheck-error-message err)) (sit-for 1))
             errors))
     (setq flycheck-highlighting-mode nil
           flycheck-display-errors-function '--flycheck-display-errors-fn)))

;; C-mode
;; ======
(add-hook 'c-mode-common-hook
             (lambda ()
               (setq c-basic-offset 4
                     tab-width 4
                     indent-tabs-mode nil)))
(setq c-default-style '(("c-mode" . "k&r")))

;; Java Mode
;; =========

;; Fix @Override indentation in Java
(add-hook 'java-mode-hook
          '(lambda ()
             "Treat Java 1.5 @-style annotations as comments."
             (setq c-comment-start-regexp
                   "\\(@\\|/\\(/\\|[*][*]?\\)\\)")
             (modify-syntax-entry ?@ "< b"
                                  java-mode-syntax-table)))

(add-hook 'java-mode-hook
	  (lambda ()
	    (setq c-basic-offset 4
		  tab-width 4
		  indent-tabs-mode nil)))

;; Python Mode
;; ===========

;; inline notification of python lines violating PEP8
(require 'flymake-python-pyflakes)
(add-hook 'python-mode-hook 'flymake-python-pyflakes-load)
(setq flymake-python-pyflakes-executable "flake8")

;; HTML Mode
;; =========

;; Map C-c C-t to a tag wrapping function when editing HTML files
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

(add-hook 'html-mode-hook (lambda () (local-set-key (kbd "C-c C-t") 'html-wrap-to-tag)))

;; Clojure & Lisp
;; ==============

(require 'paredit)
(require 'smartparens)
(require 'rainbow-delimiters)
(require 'cider)
(require 'cider-apropos)
(require 'clj-refactor)
(require 'flycheck-joker)

(defun gen-paredit-hook ()
  (paredit-mode +1))

(defun local-clojure-hook ()
  (gen-paredit-hook)
  (clj-refactor-mode 1)
  (yas-minor-mode 1)
  (cljr-add-keybindings-with-prefix "C-c C-m"))

(defun fix-paredit-delete ()
  (local-set-key (kbd "C-h") 'paredit-backward-delete))

(add-hook 'emacs-lisp-mode-hook 'gen-paredit-hook)
(add-hook 'paredit-mode-hook 'fix-paredit-delete)
(add-hook 'clojure-mode-hook 'local-clojure-hook)
(add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)
(add-hook 'clojure-test-mode-hook 'local-clojure-hook)
(add-hook 'clojure-test-mode-hook 'rainbow-delimiters-mode)
(add-hook 'cider-mode-hook 'turn-on-eldoc-mode)
(add-hook 'cider-mode-hook 'local-clojure-hook)
(add-hook 'cider-repl-mode-hook 'gen-paredit-hook)

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



(require 'puppet-mode)
(require 'markdown-mode)
(require 'yaml-mode)
(require 'web-mode)
(require 'rust-mode)
(require 'lua-mode)
(require 'go-mode)

;; Lsp
;; ===

;;(require 'lsp-mode)

;;(setq lsp-inhibit-message t
;;      lsp-eldoc-rendal-all nil
;;      lsp-highlight-symbol-at-point nil)

;;(require 'lsp-ui)
;;(require 'lsp-java)
(require 'cquery)
;;(require 'company-lsp)

;;(push 'company-lsp company-backends)
;;(setq company-lsp-async t)

;;(add-hook 'c-mode 'lsp-cquery-enable)
;;(add-hook 'c++-mode 'lsp-cquery-enable)


;;(require 'lsp-ui)
;;(require 'lsp-ui-flycheck)

;;(add-hook 'lsp-mode 'lsp-ui-mode)

;;(setq lsp-ui-sideline-enable t
;;      lsp-ui-sideline-delay 0.8
;;      lsp-ui-sideline-show-flycheck nil
;;      lsp-ui-sideline-show-hover t
;;      lsp-ui-sideline-show-symbol t
;;      lsp-ui-sideline-code-actions t
;;      lsp-ui-sideline-update-mode 'point
;;      )

;;(setq lsp-java-server-install-dir "/usr/share/java/jdtls")

;;(add-hook 'java-mode-hook 'lsp-java-enable)
(add-hook 'java-mode-hook 'flycheck-mode)
(add-hook 'java-mode-hook 'company-mode)
;;(add-hook 'java-mode-hook (lambda () (lsp-ui-flycheck-enable t)))
;;(add-hook 'java-mode-hook 'lsp-ui-sideline-mode)
;;(add-hook 'java-mode-hook (lambda () (push 'company-lsp company-backends)))

;;(setq lsp-java-organize-imports nil)
;;(setq lsp-java-save-action-organize-imports nil)

;;(setq company-lsp-enable-snippet t
;;      company-lsp-cache-candidates t)
(push 'java-mode company-global-modes)
