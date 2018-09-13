
[ -r ~/.bashrc ] && source ~/.bashrc

# settings for Mac OS
export LC_ALL=en_US.UTF-8
export LSCOLORS=gxBxhxDxfxhxhxhxhxcxcx
export EDITOR="emacsclient -t -a=\"\""
# for ctrl-x e
export ALTERNATE_EDITOR=""
export LANG=en_US.UTF-8
# Deprecated: use __git_ps1 env instead
# parse_git_branch() {
#      git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/ (\1)/'
# }
export PS1="\n\e[1;37m[\e[m\e[1;35m\u\e[m\e[1;36m@\e[m\e[1;37m\h\e[m \e[1;33m\t\e[m \w\e[m\e[1;37m]\e[m\e[1;36m\e[m\n\$(__git_ps1)$ "


source /Library/Developer/CommandLineTools/usr/share/git-core/git-prompt.sh

__pid_port() {
    lsof -P -i4TCP:$1 | grep LISTEN
}
alias pidport=" __pid_port "

function proxy_off(){
    unset http_proxy
    unset https_proxy
    echo -e "已关闭代理"
}
function proxy_on() {
    export no_proxy="localhost,127.0.0.1,localaddress,.localdomain.com"
    export http_proxy="http://127.0.0.1:8118"
    export https_proxy=$http_proxy
    echo -e "已开启代理"
}

completion_dir="$(brew --prefix)/etc/bash_completion.d"
if [[ -d $completion_dir && -r $completion_dir && -x $completion_dir ]];then
  for i in "$completion_dir"/*; do
    . $i
  done
fi

# brew install nvm
export NVM_DIR="$HOME/.nvm"
[[ -s "$NVM_DIR" ]] && source "/usr/local/opt/nvm/nvm.sh"

# curl -sSL https://get.rvm.io | bash -s stable
# Add RVM to PATH for scripting. Make sure this is the last PATH variable change.
export PATH="$PATH:$HOME/.rvm/bin"
[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm" # Load RVM into a shell session *as a function*

# java
# brew install jenv
JENV_ROOT="$HOME/.jenv"
if [ -d "${JENV_ROOT}" ]; then
  export PATH="${JENV_ROOT}/bin:$PATH"
  eval "$(jenv init -)"
  # jenv enable-plugin export
  # export JAVA_HOME="$(jenv javahome)" 通过 export 插件导出java_home
fi
# export JAVA_HOME=$(/usr/libexec/java_home) deprecated
# brew install pyenv pyenv-virtualenv
export PYENV_ROOT="${HOME}/.pyenv"
if [ -d "${PYENV_ROOT}" ]; then
    eval "$(pyenv init -)"
    eval "$(pyenv virtualenv-init -)"
else
    export PIP_REQUIRE_VIRTUALENV=true
fi

# brew
# https://lug.ustc.edu.cn/wiki/mirrors/help/homebrew-bottles
export HOMEBREW_BOTTLE_DOMAIN=https://mirrors.ustc.edu.cn/homebrew-bottles

# ocaml
. ~/.opam/opam-init/init.sh > /dev/null 2> /dev/null || true

# fix Inappropriate ioctl for device
# https://stackoverflow.com/a/41054093/2163429
export GPG_TTY=$(tty)
