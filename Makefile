ifneq 'Darwin' '$(shell uname)'
EMACS = emacs
else
EMACS = /Applications/Emacs.app/Contents/MacOS/Emacs
endif

all: autoload compile

autoload:
	$(EMACS) --chdir / --batch -l ~/.emacs.d/site-start.el -f update-all-autoloads

compile:
	$(EMACS) --chdir / --batch -l ~/.emacs.d/site-start.el -f recompile-all-configuration
