if [ "$TERM" = dumb ]; then
    export PAGER="head -n100"
    unset GIT_PAGER
    export MANPAGER="cat"
fi

export PATH=\
~/bin:\
~/langmartin/dot-file/bin:\
~/.asdf/shims:\
~/.orbstack/bin:\
~/.cargo/bin:\
~/.cabal/bin:\
~/.local/bin:\
~/contrib/flutter/bin:\
~/Library/Python/3.12/bin:\
/opt/homebrew/opt/asdf/libexec/bin:\
/opt/homebrew/opt/emacs-plus/bin:\
/opt/homebrew/opt/grep/libexec/gnubin:\
/opt/homebrew/bin:\
/opt/homebrew/sbin:\
/opt/homebrew/opt/java/bin:\
/usr/local/bin:\
/usr/bin:\
/bin:\
/usr/sbin:\
/sbin

eval "$(/usr/libexec/path_helper)"

if [ -z "$SSH_CLIENT" ]; then
    EDITOR="emacsclient"
fi

export QPVERBOSE=true
export QPOPT="-V"
export HOMEBREW_NO_AUTO_UPDATE=1
# export ASDF_DEFAULT_TOOL_VERSIONS_FILENAME=.tool-versions-local
