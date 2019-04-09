#!/bin/bash

brew install emacs --with-cocoa --with-dbus --with-librsvg --with-imagemagick@6 --with-mailutils --with-ctags

# create gtags for C/C++ reference
brew install global
# Steel Bank Common Lisp system
brew install sbcl
# Preview markdown files in a separate window, the same as on GitHub.
npm install -g vmd

# org mode resize image
brew install imagemagick@6
# org 导出 pdf 中文支持
brew cask install basictex
sudo tlmgr update --self
sudo tlmgr install wrapfig
sudo tlmgr install capt-of
# PDF 导出时，需要 cjk 包，然后需要配置org-latex-packages-alist，具体参考 setup-org.el
sudo tlmgr install cjk

# ocaml setup
opam install tuareg merlin utop

# python setup
pip install jedi flake8 autopep8


# go setup
go get -u github.com/rogpeppe/godef
go get -u github.com/nsf/gocode # for go-eldoc/company-go
go get -u golang.org/x/tools/cmd/goimports
go get -u github.com/kisielk/errcheck


# rust setup

rustup toolchain add nightly
rustup component add rust-src
cargo +nightly install racer
