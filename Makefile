.PHONY: all clean autoload compile

ifneq 'Darwin' '$(shell uname)'
EMACS = emacs
else
EMACS = /Applications/Emacs.app/Contents/MacOS/Emacs
endif

all: autoload compile

autoload:
	$(EMACS) --chdir $(CURDIR) --batch -l $(CURDIR)/site-start.el -f update-all-autoloads

compile:
	$(EMACS) --chdir $(CURDIR) --batch -l $(CURDIR)/site-start.el -f recompile-all-configuration

clean:
	find $(CURDIR) -name "*.elc" -delete
	rm -f $(CURDIR)/site-lisp/loaddefs
