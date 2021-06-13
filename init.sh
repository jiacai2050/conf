#!/bin/bash

DOTFILES_HOME=$(cd `dirname "$0"`; pwd)

command_exists () {
  type "$1" &> /dev/null ;
}

link_dotfiles_if_necessary () {
  [ -e "$HOME/$1" ] || ln -s "$DOTFILES_HOME/$1" "$HOME/$1"
}

set -x
install_emacs() {
  # https://github.com/d12frosted/homebrew-emacs-plus
  # brew tap d12frosted/emacs-plus
  # brew install emacs-plus --without-spacemacs-icon
  echo "build from source instead"
}
install_java() {
  brew cask install adoptopenjdk8 intellij-idea-ce
  brew install jenv maven clojure leiningen
  brew install borkdude/brew/clj-kondo
  jenv enable-plugin export
}

install_ocaml() {
  brew install opam ocaml
  # https://opam.ocaml.org/doc/Install.html
  # opam init
  # opam install tuareg merlin utop
}

install_rust() {
  rustup toolchain add nightly
  rustup component add rust-src
}

install_misc() {
  brew install terminal-notifier
}

install_ss() {
  brew install shadowsocks-libev privoxy
  cat << EOF > /usr/local/etc/shadowsocks-libev.json
{
    "server": "server-ip",
    "server_port": 443,
    "local_port": 1080,
    "password": "mypassword",
    "timeout": 600,
    "method": "aes-256-cfb"
}
EOF
  echo -e 'listen-address 0.0.0.0:8118\nforward-socks5 / localhost:1080 .' >> /usr/local/etc/privoxy/config
  brew services start privoxy
}

install_dbgui() {
  brew install sequel-pro robo-3t Postgres postico # mysql mongodb postgresql
}

install_vm() {
  brew cask install vagrant docker
  vagrant plugin install vagrant-disksize
  vagrant box add https://mirrors.tuna.tsinghua.edu.cn/ubuntu-cloud-images/bionic/current/bionic-server-cloudimg-amd64-vagrant.box --name tsing/bionic
  # vagrant box add https://mirrors.tuna.tsinghua.edu.cn/ubuntu-cloud-images/vagrant/trusty/current/trusty-server-cloudimg-amd64-vagrant-disk1.box --name tsing/trusty
}

install_cask() {
    # google-chrome chromium
    brew install --cask Itsycal iterm2 firefox licecap the-unarchiver microsoft-edge
}

case $1 in
  "link")
    link_dotfiles_if_necessary ".macosrc"
    link_dotfiles_if_necessary ".bashrc"
    link_dotfiles_if_necessary ".bash_profile"
    link_dotfiles_if_necessary ".emacs.d"
    link_dotfiles_if_necessary ".vim"
    link_dotfiles_if_necessary ".vimrc"
    link_dotfiles_if_necessary ".lein"
    link_dotfiles_if_necessary ".npmrc"
    link_dotfiles_if_necessary ".cargo"
    link_dotfiles_if_necessary ".zazurc.json"
    link_dotfiles_if_necessary ".gdbinit"
    link_dotfiles_if_necessary ".config"
    link_dotfiles_if_necessary ".gnupg"
    link_dotfiles_if_necessary ".eslintrc.json"
    link_dotfiles_if_necessary ".golangci.yml"
    ;;
  "brew")
    # https://lug.ustc.edu.cn/wiki/mirrors/help/brew.git
    command_exists brew || ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
    ;;
  "sm")
    git submodule update --init
    ;;
  *)
    CMD=install_$1
    declare -F "$CMD" && $CMD || echo "$CMD not exists!"
    ;;
esac
