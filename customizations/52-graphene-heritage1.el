(defun kill-default-buffer ()
  (interactive)
  (let (kill-buffer-query-functions) (kill-buffer)))

(global-set-key (kbd "C-x k") 'kill-default-buffer)

;; See https://github.com/rdallasgray/graphene/blob/master/graphene-helper-functions.el
;; for more like this

(require 'company)
(company-mode t)



;; Some common ruby file names
(dolist
    (regex
     '("\\.watchr$" "\\.arb$" "\\.rake$" "\\.gemspec$" "\\.ru$" "Rakefile$"
       "Gemfile$" "Capfile$" "Guardfile$" "Rakefile$" "Cheffile$" "Vagrantfile$"
       "Berksfile$" "\\.builder$"))
  (add-to-list 'auto-mode-alist `(,regex . ruby-mode)))

;; Smoother scrolling
(unless (and (boundp 'mac-mouse-wheel-smooth-scroll) mac-mouse-wheel-smooth-scroll)
  (global-set-key [wheel-down] (lambda () (interactive) (scroll-up-command 1)))
  (global-set-key [wheel-up] (lambda () (interactive) (scroll-down-command 1)))
  (global-set-key [double-wheel-down] (lambda () (interactive) (scroll-up-command 2)))
  (global-set-key [double-wheel-up] (lambda () (interactive) (scroll-down-command 2)))
  (global-set-key [triple-wheel-down] (lambda () (interactive) (scroll-up-command 4)))
  (global-set-key [triple-wheel-up] (lambda () (interactive) (scroll-down-command 4))))

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

(push '("\\.json\\'" . json-mode) auto-mode-alist)


;; graphene-env
;; ============

(require 'smex)
(smex-initialize)

(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

;; Use ido for general completion
(require 'flx-ido)
(ido-mode 1)
(ido-everywhere 1)
(require 'ido-ubiquitous)
(ido-ubiquitous-mode t)
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


;; Save backup files in the temporary directory
(setq backup-directory-alist `((".*" . ,temporary-file-directory))
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

;; Automatically update buffers when files change
(global-auto-revert-mode t)

;; Enable 'power user' features
(put 'dired-find-alternate-file 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
