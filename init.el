(require 'utils "~/.emacs.d/utils.el")
(require 'cask "~/.cask/cask.el")

(cask-initialize)
(load-files-in-dir "~/.emacs.d/customizations")
