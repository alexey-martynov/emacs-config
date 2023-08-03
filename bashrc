# -*- mode: shell-script -*-
if test "$(uname)" = "Darwin"
then
    export VISUAL="/Applications/Emacs.app/Contents/MacOS/bin/emacsclient -nw"
else
    export VISUAL="$(which emacsclient) -nw"
fi
