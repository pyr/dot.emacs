;;
;; Write C in BSD style
;;
(require 'dash)
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

(c-add-style
 "openbsd" ; Should be compatible with FreeBSD too
 '("bsd"
   (c-basic-offset . 8)
   (c-tab-width . 8)
   (fill-column . 80)
   (indent-tabs-mode . t)
   (c-offsets-alist . ((defun-block-intro . +)
                       (statement-block-intro . +)
                       (statement-case-intro . +)
                       (statement-cont . *)
                       (substatement-open . *)
                       (substatement . +)
                       (arglist-cont-nonempty . *)
                       (inclass . +)
                       (inextern-lang . 0)
                       (knr-argdecl-intro . +)))))

(setq c-font-lock-extra-types (-union c-font-lock-extra-types
                                      '("Gdk\\sw+" "Gtk\\sw+"
                                        "gchar" "gboolean" "guchar"
                                        "gshort" "gushort" "glong" "gulong"
                                        "gint" "gint8" "gint16" "gint32" "gint64"
                                        "guint" "guint8" "guint16" "guint32" "guint64"
                                        "glong" "gdouble" "goffset"
                                        "gsize" "gssize"
                                        "gpointer" "guintptr")))

;; Fix @Override indentation in Java
(add-hook 'java-mode-hook
          '(lambda ()
             "Treat Java 1.5 @-style annotations as comments."
             (setq c-comment-start-regexp
                   "\\(@\\|/\\(/\\|[*][*]?\\)\\)")
             (modify-syntax-entry ?@ "< b"
                                  java-mode-syntax-table)))
