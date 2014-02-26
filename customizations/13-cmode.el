(setq c-default-style '(("c-mode" . "k&r")))
(setq c-basic-offset 8)
(setq-default tab-wdith 8)
(add-hook 'c-mode-common-hook
             (lambda ()
               (setq indent-tabs-mode t)))
