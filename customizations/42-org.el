;;
;; Add support for ditaa and plantuml when editing org documents
;;
(setq org-ditaa-jar-path "/usr/share/java/ditaa/ditaa-0_9.jar")
(setq org-plantuml-jar-path "/usr/share/java/plantuml.jar")
(require 'ob-ditaa)
(require 'ob-plantuml)

(setq org-confirm-babel-evaluate
      (lambda (lang body)
        (not (string= lang "ditaa"))))

(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
