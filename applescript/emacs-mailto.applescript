on open location this_URL
	tell application "Emacs" to activate
	do shell script "/usr/local/bin/emacsclient --eval '(browse-url-mail \"" & this_URL & "\")'"
end open location
