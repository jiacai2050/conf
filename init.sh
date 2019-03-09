#!/bin/bash

DOTFILES_HOME=$(cd `dirname $0`; pwd)

command_exists () {
  type "$1" &> /dev/null ;
}
link_dotfiles_if_necessary () {
  [ -e "$HOME/$1" ] || ln -s "$DOTFILES_HOME/$1" "$HOME/$1"
}

set -x
install_java() {
  brew tap caskroom/versions
  brew cask install java8 intellij-idea-ce
  brew install jenv maven gradle
}

install_python() {
  brew install pyenv  pyenv-virtualenvwrapper pyenv-virtualenv
}

install_node() {
  brew install nvm
}

install_ruby() {
  curl -sSL https://get.rvm.io | bash -s stable
}
install_ocaml() {
  brew install opam
}

case $1 in
  "link")
    link_dotfiles_if_necessary ".bashrc"
    link_dotfiles_if_necessary ".bash_profile"
    link_dotfiles_if_necessary ".emacs.d"
    link_dotfiles_if_necessary ".vim"
    link_dotfiles_if_necessary ".vimrc"
    link_dotfiles_if_necessary ".gitconfig"
    link_dotfiles_if_necessary ".tmux.conf"
    link_dotfiles_if_necessary ".lein"
    ;;
  "brew")
    command_exists brew || ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
    ;;
  *)
    CMD=install_$1
    declare -F $CMD && $CMD || echo "$CMD not exists!"
    ;;
esac
