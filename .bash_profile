
[ -r ~/.bashrc ] && source ~/.bashrc

# settings for Mac OS
source /Library/Developer/CommandLineTools/usr/share/git-core/git-prompt.sh

__pid_port() {
    lsof -P -i4TCP:$1 | grep LISTEN
}
alias pidport=" __pid_port "

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
export GRADLE_HOME=/usr/local/opt/gradle/libexec
