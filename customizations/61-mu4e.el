(require 'mu4e-multi)
(require 'mu4e-contrib)

(setq mu4e-multi-last-read-account "exoscale")
(setq mu4e-maildir "~/.mail"
      mu4e-sent-folder "/exoscale/Sent"
      mu4e-drafts-folder "/exoscale/Drafts"
      mu4e-trash-folder "/exoscale/Trash"
      mu4e-maildir-shortcuts '(("/exoscale/inbox" . ?e)
                               ("/spootnik/inbox" . ?s))
      mu4e-get-mail-command "mbsync -a"
      mu4e-update-interval 60

      mu4e-html2text-command 'mu4e-shr2text
      mu4e-compose-signature (concat "Pierre-Yves Ritschard\n"
                                     "C.T.O - Exoscale\n"
                                     "Cell: +41 79 512 48 54.")
      message-send-mail-function 'message-send-mail-with-sendmail
      mu4e-compose-signature-auto-include nil
      mu4e-headers-include-related nil
      mu4e-compose-dont-reply-to-self t
      mu4e-user-mail-address-list '("pyr@spootnik.org"
                                    "pyr@exoscale.ch"
                                    "pyr@exoscale.com"
                                    "pierre-yves.ritschard@exoscale.ch"
                                    "pierre-yves.ritschard@exoscale.com"
                                    "prd@exoscale.ch"
                                    "pyr@openbsd.org")
      sendmail-program "/usr/bin/msmtp"
      user-full-name "Pierre-Yves Ritschard"
      user-mail-address "pyr@exoscale.com")

(setq mu4e-multi-account-alist
      '(("exoscale"
         (user-mail-address . "pyr@exoscale.com")
         (mu4e-drafts-folder . "/exoscale/Drafts")
         (mu4e-sent-folder . "/exoscale/Sent")
         (mu4e-trash-folder . "/exoscale/Trash"))
        ("spootnik"
         (user-mail-address . "pyr@spootnik.org")
         (mu4e-drafts-folder . "/spootnik/Drafts")
         (mu4e-sent-folder . "/spootnik/Sent")
         (mu4e-trash-folder . "/spootnik/Trash"))))

(add-to-list 'mu4e-bookmarks
             '("flag:list and flag:unread" "Mailing Lists" ?m))
(add-to-list 'mu4e-bookmarks
             '("maildir:/spootnik/inbox" "Spootnik Inbox" ?s))
(add-to-list 'mu4e-bookmarks
             '("maildir:/exoscale/inbox" "Exoscale Inbox" ?e))
(add-to-list 'mu4e-bookmarks
             '("maildir:/exoscale/inbox or maildir:/spootnik/inbox" "Combined Inbox" ?b))

(define-key mu4e-headers-mode-map "C" 'mu4e-multi-compose-new)
(add-hook 'message-send-mail-hook 'mu4e-multi-smtpmail-set-msmtp-account)

(setq mu4e-change-filenames-when-moving t)


(mu4e-multi-enable)
