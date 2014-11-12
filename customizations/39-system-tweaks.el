(when (and window-system (string-match "apple-darwin" system-configuration))
  (setq graphene-default-font "Inconsolata 14")
  (setq graphene-fixed-pitch-font "Inconsolata 14")
  (set-face-attribute 'default nil :font "Inconsolata 14"))
