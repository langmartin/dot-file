1. this script, saved with /Applications/Utilities/Script Editor.app and not Automator

```
on open location this_URL
	tell application "Emacs" to activate
	do shell script "/usr/local/bin/emacsclient --eval '(browse-url-mail \"" & this_URL & "\")'"
end open location
```

2. export as an application bundle
3. edit info.plist to contain

```
<key>CFBundleURLTypes</key>
<array>
	<dict>
		<key>CFBundleURLName</key>
		<string>Email Address URL</string>
		<key>CFBundleURLSchemes</key>
		<array>
			<string>mailto</string>
		</array>
	</dict>
</array>
```

4. in Mail.app, select this new application bundle as the default mail
   handler
