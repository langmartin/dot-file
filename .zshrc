set -a
PROMPT='%T %? %m%# '
WORDCHARS="*?[]~=&;!#$%^(){}<>"
HISTSIZE=5000
SAVEHIST=5000
HISTFILE=~/.zhistory

alias ll='ls -l'
alias s='git status'
alias b='git branch -v'
alias ba='git branch -av'
alias grunt='grunt --no-color'
alias cn='cd ~/go/src/github.com/hashicorp/nomad'
alias cc='cd ~/go/src/github.com/hashicorp/consul'

setopt HIST_IGNORE_ALL_DUPS
setopt INC_APPEND_HISTORY
[ "$TERM" = dumb ] && unsetopt zle

dockerize () {
    eval `docker-machine env default`
}

anybar () {
    [ -n "$1" ] && c="$1" || c=white
    echo -n "$c" | nc -4u -w0 localhost ${2:-1738}
}

make () {
    anybar
    env make $@ && anybar green || anybar red
}

. ~/.opam/opam-init/init.zsh > /dev/null 2> /dev/null || true
