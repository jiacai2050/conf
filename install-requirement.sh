#!/bin/bash

# build from source instead
# brew install emacs --with-cocoa --with-dbus --with-librsvg --with-imagemagick@6 --with-mailutils --with-ctags

# create gtags for C/C++ reference
brew install global
# Steel Bank Common Lisp system
brew install sbcl
# Preview markdown files in a separate window, the same as on GitHub.
npm install -g vmd

# org 导出 pdf 中文支持
# brew cask install basictex
# sudo tlmgr update --self
# sudo tlmgr install wrapfig
# sudo tlmgr install capt-of
# PDF 导出时，需要 cjk 包，然后需要配置org-latex-packages-alist，具体参考 setup-org.el
# sudo tlmgr install cjk

npm install jsonlint -g

go get golang.org/x/tools/gopls@latest

# project management tool for Emacs
# brew install cask
curl -fsSL https://gitee.com/liujiacai/cask/raw/master/go | python

# https://www.gnu.org/software/emacs/manual/html_node/emacs/Spelling.html
brew install ispell
