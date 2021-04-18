#!/bin/bash

function command_exists() {
    command -v "$1" &> /dev/null
}
# build from source instead
# brew install emacs --with-cocoa --with-dbus --with-librsvg --with-imagemagick@6 --with-mailutils --with-ctags

set -ex
# create gtags for C/C++ reference
command_exists global || brew install global
command_exists clangd || brew install llvm
# Steel Bank Common Lisp system
command_exists sbcl || brew install sbcl
# Preview markdown files in a separate window, the same as on GitHub.
command_exists vmd || npm install -g vmd

# project management tool for Emacs
# brew install cask
command_exists cask || curl -fsSL https://gitee.com/liujiacai/cask/raw/master/go | python

# https://www.gnu.org/software/emacs/manual/html_node/emacs/Spelling.html
command_exists aspell || brew install aspell

command_exists rg || brew install ripgrep
command_exists pngpaste || brew install pngpaste
command_exists gtimeout || brew install coreutils

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

function install_staticcheck() {
  version="2020.2.1"
  curl -Lo /tmp/staticcheck.tar.gz "https://github.com/dominikh/go-tools/releases/download/${version}/staticcheck_darwin_amd64.tar.gz"
  pushd /tmp
  tar xf /tmp/staticcheck.tar.gz
  cp "/tmp/staticcheck/staticcheck" /usr/local/bin/
  staticcheck --version
  popd
}

command_exists eslint || npm install -g eslint
# LSP
command_exists gopls || go get golang.org/x/tools/gopls@latest
# https://github.com/palantir/python-language-server
command_exists pyls || pip install 'python-language-server[all]'
# https://github.com/theia-ide/typescript-language-server
command_exists typescript-language-server || (npm i -g typescript-language-server && npm i -g typescript)


# Golang
# command_exists staticcheck || install_staticcheck
command_exists goimports || go get -u golang.org/x/tools/cmd/goimports
command_exists gorename || go get -u golang.org/x/tools/cmd/gorename

# Mail
command_exists mu || brew install mu offlineimap
mail_dir="$HOME/.mail/"
[ -d "${mail_dir}" ] || mkdir -p "${mail_dir}"
