EMACS = emacs

all: autoload

autoload:
	$(EMACS) --chdir / --batch -l ~/.emacs.d/site-start.el -f update-all-autoloads
