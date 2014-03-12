;;
;; In most places emacs relies on the shell to be
;; bash/ksh, this avoids quirks when your shell isn't
;;
(setq shell-file-name "/bin/bash")
(add-to-list 'process-environment "SHELL=/bin/bash")
