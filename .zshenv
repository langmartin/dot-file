if [ "$TERM" = dumb ]; then
    export PAGER="head -n100"
    unset GIT_PAGER
    export MANPAGER="cat"
fi

add_path () {
    for p in "$@"; do
        case ":$PATH:" in
            *:"$d":*) ;;
            *) [ -d "$p" ] && PATH="$p:$PATH" ;;
        esac
    done
}

if [ -z "$INSIDE_EMACS" ]; then
    add_path \
        /sbin \
        /usr/sbin \
        /usr/local/texlive/2022/bin/universal-darwin \

    add_path \
        /usr/local/bin \
        /usr/local/sbin \
        /opt/homebrew/opt/java/bin \
        /opt/homebrew/bin \
        /opt/homebrew/sbin \

    add_path \
        ~/.asdf/shims \
        ~/.cargo/bin \
        ~/.cabal/bin \
        ~/go/bin \
        ~/.local/bin \
        ~/langmartin/dot-file/bin \
        ~/bin \

fi

if [ -z "$SSH_CLIENT" ]; then
    EDITOR="emacsclient"
fi

export VERITAS_HOME=~/subspace/veritas
export QPVERBOSE=true
export QPOPT="-V"
export HOMEBREW_NO_AUTO_UPDATE=1

[ -f "$HOME/.cargo/env" ] && \
    source "$HOME/.cargo/env"
