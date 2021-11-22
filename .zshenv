if [ "$TERM" = dumb ]; then
    export PAGER="head -n100"
    unset GIT_PAGER
    export MANPAGER="cat"
fi

if [ -z "$INSIDE_EMACS" ]; then
    SYS_PATH="$PATH"
    PATH=~/bin
    PATH="$PATH":~/langmartin/dot-files/bin
    PATH="$PATH":~/subspace/effitas/federations/holodeck/dev-scripts
    PATH="$PATH":/usr/local/texlive/2018/bin/x86_64-darwin
    PATH="$PATH":~/code/contrib/google-cloud-sdk/bin
    PATH="$PATH":~/.cargo/bin
    PATH="$PATH":/usr/local/go/bin
    PATH="$PATH":~/go/bin
    PATH="$PATH":/Users/lang/.gem/ruby/2.6.0/bin
    PATH="$PATH:/usr/local/bin"
    PATH="$PATH:/usr/local/sbin"
    PATH="$PATH:$SYS_PATH:/sbin:/usr/sbin"
fi

if [ -z "$SSH_CLIENT" ]; then
    EDITOR="/usr/local/bin/emacsclient"
fi

export VERITAS_HOME=~/subspace/veritas
export QPVERBOSE=true
export QPOPT="-V"
export HOMEBREW_NO_AUTO_UPDATE=1

source "$HOME/.cargo/env"
