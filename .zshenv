if [ "$TERM" = dumb ]; then
    export PAGER="head -n100"
    unset GIT_PAGER
    export MANPAGER="cat"
fi

if [ -z "$INSIDE_EMACS" ]; then
    SYS_PATH="$PATH"
    PATH=~/bin
    PATH="$PATH":~/langmartin/dot-file/bin
    PATH="$PATH":~/subspace/effitas/federations/holodeck/dev-scripts
    PATH="$PATH":/usr/local/texlive/2018/bin/x86_64-darwin
    PATH="$PATH":/opt/homebrew/texlive/2018/bin/x86_64-darwin
    PATH="$PATH":~/code/contrib/google-cloud-sdk/bin
    PATH="$PATH":~/.asdf/shims
    PATH="$PATH":~/.cargo/bin
    PATH="$PATH":~/go/bin
    PATH="$PATH":~/code/contrib/google-cloud-sdk/bin
    PATH="$PATH":/usr/local/texlive/2018/bin/x86_64-darwin
    PATH="$PATH":/usr/local/opt/openjdk/bin
    PATH="$PATH":/Users/lang/.gem/ruby/2.6.0/bin
    PATH="$PATH":/usr/local/bin
    PATH="$PATH":/opt/homebrew/bin
    PATH="$PATH":/usr/local/sbin
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
