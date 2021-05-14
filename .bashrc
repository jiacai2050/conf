[ -r ~/.macosrc ] && source ~/.macosrc
# 别名相关配置
[ -r ~/.aliasrc ] && source ~/.aliasrc
# PATH 相关配置
[ -r ~/.pathrc ] && source ~/.pathrc
# 自己程序中的相关配置
[ -r ~/.devrc ] && source ~/.devrc

# brew install bash-completion
# https://computingforgeeks.com/how-to-fix-get-comp-words-by-ref-command-not-found/
bash_comp_sh="/usr/local/etc/profile.d/bash_completion.sh"
# [ -r "$bash_comp_sh" ] && . "$bash_comp_sh"
# Bash completion has been installed to:
# /usr/local/etc/bash_completion.d

export LC_ALL=en_US.UTF-8
#export EDITOR="emacsclient -t -a=\"\""
# for ctrl-x e
export VISUAL=vim
export EDITOR="$VISUAL"

export ALTERNATE_EDITOR=""
export LANG=en_US.UTF-8

function proxy_off(){
    unset all_proxy
    unset http_proxy
    unset https_proxy
    echo -e "已关闭代理"
}
function proxy_on() {
    export no_proxy="localhost,127.0.0.1,localaddress,.localdomain.com,192.168.33.10,.alipay-inc.com"
    # export http_proxy="http://127.0.0.1:8118"
    export http_proxy="socks5://127.0.0.1:1080"
    export http_proxy="socks5://127.0.0.1:13659"
    export https_proxy=$http_proxy
    export all_proxy=$http_proxy
    echo -e "已开启代理"
}

function command_exists() {
    command -v "$1" &> /dev/null
}

alias ls='ls -FG'
alias ll='ls -lh'
alias cd.='cd ..'
alias cd..='cd ../..'
alias cd...='cd ../../..'
alias gc='git clone '
alias gf='git diff --color | diff-so-fancy'
# alias e='emacsclient -t -a ""'
alias tailf='tail -F '
alias tf='tail -F '
alias t='tail '
alias dl='cd ~/Downloads'
alias p='ping qq.com'
alias gpg-kill='gpgconf --kill gpg-agent'
alias pgrep='pgrep -l '

# https://metaredux.com/posts/2020/07/07/supercharge-your-bash-history.html
# don't put duplicate lines or lines starting with space in the history.
# See bash(1) for more options
HISTCONTROL=ignoreboth

# append to the history file, don't overwrite it
shopt -s histappend
# append and reload the history after each command
PROMPT_COMMAND="history -a"

# ignore certain commands from the history
HISTIGNORE="ls:ll:cd:pwd:bg:fg:history:cd.:cd..:cd...:e"

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=100000
HISTFILESIZE=10000000

# export RUSTUP_DIST_ROOT="https://static.rust-lang.org/dist"
# export RUSTUP_DIST_SERVER="https://static.rust-lang.org"
export RUSTUP_DIST_SERVER="https://mirrors.tuna.tsinghua.edu.cn/rustup"
export RUSTUP_UPDATE_ROOT="https://mirrors.ustc.edu.cn/rust-static/rustup"
export RUSTUP_DIST_SERVER="https://mirrors.ustc.edu.cn/rust-static"
export PATH=$PATH:$HOME/.cargo/bin

# go
export GOPATH="$HOME/code/go"
export GO111MODULE=on
export GOPROXY="https://goproxy.cn,direct"
export GOPRIVATE=gitlab.alipay-inc.com,gitlab.alibaba-inc.com,code.alipay.com
export PATH=$PATH:$GOPATH/bin
# REPL
alias yaegi='rlwrap yaegi'
alias gobx='GOOS=linux GOARCH=amd64 go build -v '

# misc
POSTGRES_BIN=/Applications/Postgres.app/Contents/Versions/latest/bin
export PATH=/usr/local/opt/llvm/bin:$PATH:$POSTGRES_BIN:$HOME/bin:$HOME/.cask/bin

# mu
export XAPIAN_CJK_NGRAM=true
