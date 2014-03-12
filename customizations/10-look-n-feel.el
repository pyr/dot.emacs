;;
;; Provide a smoother look, by removing most GUI elements:
;; - no scrollbars
;; - no menubar
;; - no toolbar
;; - no tooltips
;; - show matching parens and braces
;; - prevent the emacs splash screen from being displayed
;;

(menu-bar-mode -1)
(global-font-lock-mode t)
(column-number-mode t)
(show-paren-mode t)
(setq inhibit-splash-screen t)
(blink-cursor-mode t)
(set-fringe-style -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(delete-selection-mode t)
(fset 'yes-or-no-p 'y-or-n-p)
(setq make-backup-files nil)
(setq auto-save-default nil)
