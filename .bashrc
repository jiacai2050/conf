export EDITOR="emacsclient -t -a=\"\""
# for ctrl-x e
export ALTERNATE_EDITOR=""
export LANG=en_US.UTF-8
parse_git_branch() {
     git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/ (\1)/'
}
export PS1="\n\e[1;37m[\e[m\e[1;35m\u\e[m\e[1;36m@\e[m\e[1;37m\h\e[m \e[1;33m\t\e[m \w\e[m\e[1;37m]\e[m\e[1;36m\e[m\n\$(__git_ps1)$ "

# 别名相关配置
[ -r ~/.aliasrc ] && source ~/.aliasrc
# PATH 相关配置
[ -r ~/.pathrc ] && source ~/.pathrc
# 自己程序中的相关配置
[ -r ~/.devrc ] && source ~/.devrc
