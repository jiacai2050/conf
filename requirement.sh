#!/bin/bash

function command_exists() {
    command -v "$1" &> /dev/null
}
# build from source instead
# brew install emacs --with-cocoa --with-dbus --with-librsvg --with-imagemagick@6 --with-mailutils --with-ctags

set -ex
# create gtags for C/C++ reference
command_exists global || brew install global
# Steel Bank Common Lisp system
command_exists sbcl || brew install sbcl
# Preview markdown files in a separate window, the same as on GitHub.
command_exists vmd || npm install -g vmd

command_exists jsonlint || npm install jsonlint -g

# project management tool for Emacs
# brew install cask
command_exists cask || curl -fsSL https://gitee.com/liujiacai/cask/raw/master/go | python

# https://www.gnu.org/software/emacs/manual/html_node/emacs/Spelling.html
command_exists ispell || brew install ispell

# flycheck
function install_shellcheck() {
  scversion="v0.7.1" # or "v0.4.7", or "latest"
  curl -Lo /tmp/sc.tar.xz "https://github.com/koalaman/shellcheck/releases/download/${scversion?}/shellcheck-${scversion?}.darwin.x86_64.tar.xz"
  pushd /tmp
  tar xf /tmp/sc.tar.xz
  cp "/tmp/shellcheck-${scversion}/shellcheck" /usr/local/bin/
  shellcheck --version
  popd
}
command_exists shellcheck || install_shellcheck

# LSP
command_exists gopls || go get golang.org/x/tools/gopls@latest
# https://github.com/palantir/python-language-server
command_exists pyls || pip install 'python-language-server[all]'
