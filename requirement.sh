#!/bin/bash

function command_exists() {
    command -v "$1" &> /dev/null
}

function font_installed() {
    name="$1"
    fc-match "$name" | grep "$name"
}

set -ex

command_exists go || brew install go
command_exists node || brew install node
command_exists rustup || brew install rustup

# create gtags for C/C++ reference
command_exists global || brew install global
command_exists clangd || brew install llvm

# Steel Bank Common Lisp system
command_exists sbcl || brew install sbcl

# Python
command_exists pyenv || brew install pyenv pyenv-virtualenv
command_exists black || pip install black

# Markdown
command_exists vmd || npm install -g vmd
command_exists multimarkdown || brew install multimarkdown

# project management tool for Emacs
# brew install cask
command_exists cask || curl -fsSL https://gitee.com/liujiacai/cask/raw/master/go | python

# https://www.gnu.org/software/emacs/manual/html_node/emacs/Spelling.html
command_exists aspell || brew install aspell

# Utils
command_exists pinentry || brew install pinentry
command_exists tmux || brew install tmux
command_exists rg || brew install ripgrep
command_exists pngpaste || brew install pngpaste
command_exists gtimeout || brew install coreutils
# http://www.figlet.org/examples.html
command_exists figlet || brew install figlet

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

command_exists eslint || npm install -g eslint
# LSP
command_exists gopls || go get golang.org/x/tools/gopls@latest
command_exists pyright || npm install -g pyright

# Nodejs.
# https://github.com/theia-ide/typescript-language-server
command_exists typescript-language-server || (npm i -g typescript-language-server && npm i -g typescript)

# Golang
command_exists golangci-lint|| go get  -v github.com/golangci/golangci-lint/cmd/golangci-lint@v1.40.1
command_exists goimports || go get -u -v golang.org/x/tools/cmd/goimports

# Mail
command_exists mu || brew install mu offlineimap
mail_dir="$HOME/.mail/"
[ -d "${mail_dir}" ] || mkdir -p "${mail_dir}"

font_installed "SF Mono" || brew install font-sf-mono
font_installed "Hack" || brew install font-hack
