dot.emacs: a simple emacs configuration layout
==============================================

This is my emacs config meant to be easily synchronised across MacOS
and Linux machines.

![emacs config](http://i.imgur.com/45V15Pp.png)

### First time set-up

You will want a recent version of emacs installed:

- ubuntu users will want to use this ppa: https://launchpad.net/~cassou/+archive/emacs
- archlinux users will have access to emacs 24.3 through pacman
- MacOS users will want to install from: http://emacsformacosx.com/

This configuration relies on [cask](http://cask.github.io), please refer to
http://cask.github.io/installation to get cask installed.

Once cask is set-up you can just clone this directory in your
`$HOME/.emacs.d`:

```
cd $HOME && git clone https://github.com/pyr/dot.emacs .emacs.d
```

You can then install dependencies by running (within `$HOME/.emacs.d`):

```
cask install
```

### Curating your list of packages

The `$HOME/.emacs.d/Cask` file contains the list of dependencies you
wish to pull in. You will most likely want to fork this repository and
maintain your own list of dependencies, since this it will be
dependent on your workflow and personal preference.

### Customizations

Customization may be provided in the `$HOME/.emacs.d/customizations`
directory. Only `.el` files prefixed with two digits will be
interpreted.

Some notable out of the box customizations:

- Solarized Dark and Light themes switchable with `<control>-c t`
- Graphene for smoother fonts and small theme improvements
- paredit and cider for great clojure support (improves emacs-lisp
  too)
- lots of editing modes:
  python/ruby/haskell/coffee/markdown/clojure/json/puppet
- a blogging engine/static site generator:
  [o-blog](http://renard.github.io)
- inline repl for haskell, clojure and python notebooks
- ag integration for fast in-project searches
- [gist](https://gist.github.com) integration for quick buffer sharing
- great git integration thanks to magit

### Updating dependencies

Dependencies may be updated by running `cask update`. Cask itself
may be updated by running `cask upgrade`.
