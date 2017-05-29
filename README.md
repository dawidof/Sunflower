# Sunflower emacs configuration

### How to install on mac

The easiest way to install via [brew](https://brew.sh/)

```bash
brew install emacs --with-cocoa -with-ctags --with-dbus --with-gnutls --with-imagemagick@6 --with-librsvg --with-mailutils --with-modules --HEAD
```

if You got previous configuration you can move

```bash
mv ~/.emacs.d ~/.emacs.d.bak
```
 or delete it

```bash
rm -rf ~/.emacs.d
```

Then, clone this repository in ~.emacs.d~

```bash
git clone https://github.com/dawidof/sunflower.git ~/.emacs.d
```

If you are updating

```bash
cd ~/.emacs.d && git pull
```
