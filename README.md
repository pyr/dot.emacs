dot.emacs: a simple emacs configuration layout
==============================================

This is my emacs config meant to be easily synchronised across MacOS
and Linux machines.

### First time set-up

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

### Updating dependencies

Dependencies may be updated by running `cask update`. Cask itself
may be updated by running `cask upgrade`.
