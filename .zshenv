PATH="/usr/local/bin:$PATH:/sbin:/usr/sbin:/usr/local/sbin:/usr/local/share/npm/bin:"~/bin

if [ "$TERM" = dumb ]; then
	export PAGER="head -n100"
	unset GIT_PAGER
	export MANPAGER="cat"
fi

if [ -z "$SSH_CLIENT" ]; then
	EDITOR="/usr/local/bin/emacsclient"
fi
