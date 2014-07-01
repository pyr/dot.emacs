;;
;; Write C in BSD style
;;
(setq c-default-style '(("c-mode" . "k&r")))
;;(setq c-basic-offset 8)
;;(setq-default tab-wdith 8)
(add-hook 'c-mode-common-hook
             (lambda ()
               (setq c-basic-offset 4
                     tab-width 4
                     indent-tabs-mode nil)))

(add-hook 'java-mode-hook
                (lambda ()
                  (setq c-basic-offset 4
                        tab-width 4
                        indent-tabs-mode nil)))
