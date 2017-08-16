set -a
PROMPT='%T %? %m%# '
WORDCHARS="*?[]~=&;!#$%^(){}<>"
HISTSIZE=5000
SAVEHIST=5000
HISTFILE=~/.zhistory

alias ll='ls -l'
alias s='git status'
alias b='git branch -av'
alias grunt='grunt --no-color'

setopt HIST_IGNORE_ALL_DUPS
setopt INC_APPEND_HISTORY
[ "$TERM" = dumb ] && unsetopt zle

dockerize () {
    eval `docker-machine env default`
}

anybar () {
    echo -n "$1" | nc -4u -w0 localhost ${2:-1738}
}

make () {
    env make $@ && anybar green || anybar red
}

. /Users/lang/.opam/opam-init/init.zsh > /dev/null 2> /dev/null || true
