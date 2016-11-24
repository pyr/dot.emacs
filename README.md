dot.emacs: a simple emacs configuration layout
==============================================

[![Build Status](https://secure.travis-ci.org/pyr/dot.emacs.png)](http://travis-ci.org/pyr/dot.emacs)


This is my emacs config meant to be easily synchronised across MacOS
and Linux machines.

![emacs config](http://i.imgur.com/45V15Pp.png)

### First time set-up

You will want a recent (24+) version of emacs installed:

This configuration relies on [emenv](https://github.com/pyr/emenv), please install
it.

Once emenv is set-up you can just clone this directory in your
`$HOME/.emacs.d`:

```
cd $HOME && git clone https://github.com/pyr/dot.emacs .emacs.d
```

You can then install dependencies by running (within `$HOME/.emacs.d`):

```
emenv install
```

### Curating your list of packages

The `$HOME/.emacs.d/Emenv` file contains the list of dependencies you
wish to pull in. You will most likely want to fork this repository and
maintain your own list of dependencies, since this it will be
dependent on your workflow and personal preference.

### Customizations

Customization may be provided in the `$HOME/.emacs.d/customizations`
directory. Only `.el` files prefixed with two digits will be
interpreted.

Some notable out of the box customizations:

- Solarized Dark and Light themes switchable with `<control>-c t`
- Paredit and cider for great clojure support (improves emacs-lisp
  too)
- Lots of editing modes:
  python/ruby/haskell/coffee/markdown/clojure/json/puppet
- Inline repl for haskell, clojure and python notebooks
- ag integration for fast in-project searches
- [gist](https://gist.github.com) integration for quick buffer sharing
- Great git integration thanks to magit

### Updating dependencies

Dependencies may be updated by running `emenv sync` then `emenv install`.
