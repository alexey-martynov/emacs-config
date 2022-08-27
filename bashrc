# -*- mode: shell-script -*-
if test "$(uname)" = "Darwin"
then
    export VISUAL="/Applications/Emacs.app/Contents/MacOS/bin/emacsclient -nw"
else
    export VISUAL="/usr/bin/emacsclient -nw"
fi
