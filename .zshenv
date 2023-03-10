if [ "$TERM" = dumb ]; then
    export PAGER="head -n100"
    unset GIT_PAGER
    export MANPAGER="cat"
fi

add_path () {
    for p in "$@"; do
        [ -d "$p" ] && PATH="$PATH:$p"
    done
}

if [ -z "$INSIDE_EMACS" ]; then
    SYS_PATH="$PATH"

    add_path \
        ~/bin \
        ~/langmartin/dot-file/bin \
        ~/.asdf/shims \
        ~/.cargo/bin \
        ~/.cabal/bin \
        ~/go/bin \
        ~/.local/bin

    add_path \
        /opt/homebrew/bin \
        /opt/homebrew/sbin \
        /opt/homebrew/opt/java/bin \
        /usr/local/bin \
        /usr/local/sbin

    add_path \
        /usr/local/texlive/2022/bin/universal-darwin

    PATH="$PATH:$SYS_PATH":/sbin:/usr/sbin
fi

if [ -z "$SSH_CLIENT" ]; then
    EDITOR="/usr/local/bin/emacsclient"
fi

export VERITAS_HOME=~/subspace/veritas
export QPVERBOSE=true
export QPOPT="-V"
export HOMEBREW_NO_AUTO_UPDATE=1

[ -f "$HOME/.cargo/env" ] && \
    source "$HOME/.cargo/env"
