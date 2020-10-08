set -a
PROMPT='%* %? %m%# '
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
alias cm='cd ~/go/src/github.com/langmartin/nomad-dev'
alias cu='cd ~/go/src/github.com/hashicorp/nomad/e2e/upgrades'
alias cc='cd ~/go/src/github.com/hashicorp/consul'

setopt HIST_IGNORE_ALL_DUPS
setopt INC_APPEND_HISTORY
[ "$TERM" = dumb ] && unsetopt zle

dockerize () {
    eval `docker-machine env default`
}

AnyBarHost=localhost
# AnyBarHost=10.199.0.1
anybar () {
    [ -n "$1" ] && c="$1" || c=white
    echo -n "$c" | nc -4u -w0 $AnyBarHost ${2:-1738}
}

make () {
    anybar
    env make $@ && anybar green || anybar red
}

for inc in \
    ~/.opam/opam-init/init.zsh \
	/usr/local/opt/asdf/asdf.sh \
	~/.secret/sh \
    ; do
    . "$inc" > /dev/null 2> /dev/null || true
done

if `which kubectx`; then
    KUBECTX=`kubectx -c`
    alias kubectx () {
	/usr/local/bin/kubectx "$@"
	KUBECTX=`/usr/local/bin/kubectx -c`
    }
    setopt prompt_subst
    PROMPT='$KUBECTX '"$PROMPT"
fi

netstat-lpn () {
    sudo lsof -nP -iTCP -sTCP:LISTEN
}
