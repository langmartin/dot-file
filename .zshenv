if [ "$TERM" = dumb ]; then
    export PAGER="head -n100"
    unset GIT_PAGER
    export MANPAGER="cat"
fi

if [ -z "$INSIDE_EMACS" ]; then
    SYS_PATH="$PATH"
    PATH=~/bin
    PATH="$PATH":/usr/local/texlive/2018/bin/x86_64-darwin
    PATH="$PATH":~/code/contrib/google-cloud-sdk/bin
    PATH="$PATH":~/.cargo/bin
    PATH="$PATH":/usr/local/go/bin
    PATH="$PATH":~/go/bin
    PATH="$PATH:/usr/local/bin"
    PATH="$PATH:/usr/local/sbin"
    PATH="$PATH:$SYS_PATH:/sbin:/usr/sbin"
fi

if [ -z "$SSH_CLIENT" ]; then
    EDITOR="/usr/local/bin/emacsclient"
fi

export QPVERBOSE=true
export QPOPT="-V"

