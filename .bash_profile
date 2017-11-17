
[ -r ~/.bashrc ] && source ~/.bashrc

export LC_ALL=en_US.UTF-8
export LSCOLORS=gxBxhxDxfxhxhxhxhxcxcx

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

# java
export JAVA_HOME=$(/usr/libexec/java_home)

# brew install nvm
export NVM_DIR="$HOME/.nvm"
[[ -s "$NVM_DIR" ]] && source "/usr/local/opt/nvm/nvm.sh"

# curl -sSL https://get.rvm.io | bash -s stable
# Add RVM to PATH for scripting. Make sure this is the last PATH variable change.
export PATH="$PATH:$HOME/.rvm/bin"
[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm" # Load RVM into a shell session *as a function*

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
