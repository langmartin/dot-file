set -a
PROMPT='%* %? %m%# '
WORDCHARS="*?[]~=&;!#$%^(){}<>"
HISTSIZE=700000
SAVEHIST=500000
HISTFILE=~/.zhistory

function h () {grep "$@" ~/.zhistory | tail;}
alias ll='ls -l'
alias s='git status'
alias b='git branch -v'
alias ba='git branch -av'
alias grunt='grunt --no-color'
alias firefox="TZ=America/Los_Angeles open /Applications/Firefox.app"
alias slack="TZ=America/Los_Angeles open /Applications/Slack.app"
alias book='(cd ~/langmartin/mkelixir/Book && JAVA_HOME=/opt/homebrew/opt/openjdk@11 rake book.pdf && open book.pdf)'
alias dotenv='set -a; . ./.env; set +a'

ulimit -n 12288

setopt HIST_EXPIRE_DUPS_FIRST
setopt HIST_IGNORE_DUPS
setopt INC_APPEND_HISTORY_TIME
setopt HIST_REDUCE_BLANKS
# command with a leading space won't be recorded in the history
setopt HIST_IGNORE_SPACE
# [ "$TERM" = dumb ] && unsetopt zle

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

fs () {
    awk "{print \$$1;}"
}

function jwt() {
    jq -R 'split(".") | .[1] | @base64d | fromjson' <<< "$1"
}

netstat-lpn () {
    sudo lsof -nP -iTCP -sTCP:LISTEN
}

alias nocolor='echo "\e[0m"'

for inc in \
    ~/.opam/opam-init/init.zsh \
	/usr/local/opt/asdf/asdf.sh \
        /opt/homebrew/opt/asdf/libexec/asdf.sh \
	~/.secret/sh \
    ; do
    . "$inc" > /dev/null 2> /dev/null || true
done

setopt prompt_subst

git-current-branch () {
    v=`git rev-parse --abbrev-ref HEAD 2>/dev/null` && echo "$v "
}

PROMPT='$(git-current-branch)'"$PROMPT"

if [ -x /usr/local/bin/kubectx ]; then
    kubectx-current () {
	v=`/usr/local/bin/kubectx -c 2>/dev/null` && echo "$v "
    }
    PROMPT='$(kubectx-current)'"$PROMPT"
fi

export NVM_DIR="$HOME/.nvm"
[ -s "/usr/local/opt/nvm/nvm.sh" ] && \. "/usr/local/opt/nvm/nvm.sh"
