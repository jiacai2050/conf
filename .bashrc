export EDITOR="emacsclient -t -a=\"\""
# for ctrl-x e
export ALTERNATE_EDITOR=""
export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8
export LSCOLORS=gxBxhxDxfxhxhxhxhxcxcx
export PS1="\n\e[1;37m[\e[m\e[1;35m\u\e[m\e[1;36m@\e[m\e[1;37m\h\e[m \e[1;33m\t\e[m \w\e[m\e[1;37m]\e[m\e[1;36m\e[m\n\$ "

# 编程语言相关
export JAVA_HOME=$(/usr/libexec/java_home)
export PIP_REQUIRE_VIRTUALENV=true

# 别名相关配置
[ -r ~/.aliasrc ] && source ~/.aliasrc
# PATH 相关配置
[ -r ~/.pathrc ] && source ~/.pathrc
# 自己程序中的相关配置
[ -r ~/.devrc ] && source ~/.devrc

# brew install nvm
export NVM_DIR="$HOME/.nvm"
. "/usr/local/opt/nvm/nvm.sh"

