;;(require 'calfw)
;;(require 'calfw-org)
;;(require 'org-gcal)

;;(let* ((path          "~/.emacs.d/.gcal-credentials")
;;       (calendar-path "~/Documents/Dropbox/Perso/Org/main-calendar.org")
;;       (creds         (with-temp-buffer
;;                        (insert-file-contents-literally path)
;;                        (goto-char 0)
;;                        (read (buffer-string))))
;;       (client-id     (car creds))
;;       (client-secret (car (cdr creds))))
;;  (setq org-gcal-client-id     client-id
;;        org-gcal-client-secret client-secret
;;        org-gcal-file-alist    `(("pyr@spootnik.org" . ,calendar-path))
;;        org-agenda-files       `(,calendar-path)))
