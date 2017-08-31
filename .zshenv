PATH="/usr/local/bin:$PATH:/sbin:/usr/sbin"
PATH="$PATH":~/bin:~/code/dot-file/bin
PATH="$PATH":~/code/contrib/google-cloud-sdk/bin

if [ "$TERM" = dumb ]; then
	export PAGER="head -n100"
	unset GIT_PAGER
	export MANPAGER="cat"
fi

if [ -z "$SSH_CLIENT" ]; then
	EDITOR="/Applications/Emacs.app/Contents/MacOS/bin/emacsclient"
fi

export QPVERBOSE=true
export QPOPT="-V"

