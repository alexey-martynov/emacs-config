# -*- mode: shell-script -*-
if "$(uname)" = "Darwin"
then
    export VISUAL="/Applications/Emacs.app/Contents/MacOS/bin/emacsclient -nw"
else
    export VISUAL="/usr/bin/emacsclient -nw"
fi
