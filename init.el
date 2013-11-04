(require 'cask "~/.cask/cask.el")

(cask-initialize)
(mapc 'load (directory-files "~/.emacs.d/customizations" t "^[0-9]+.*\.el$"))

