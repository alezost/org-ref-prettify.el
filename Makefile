# This is not a full-featured Makefile and it is not intended to be used
# to install this package to your system.  Its only purpose is to
# byte-compile "org-ref-prettify.el" (using 'make') - to make sure that
# there are no compilation warnings;

TOP := $(dir $(lastword $(MAKEFILE_LIST)))
ELPA_DIR = $(HOME)/config/emacs/data/elpa

LOAD_PATH = -L $(TOP)
LOAD_PATH += $(shell \
  find $(ELPA_DIR) -mindepth 1 -maxdepth 1 -type d | \
  xargs -I === echo "-L ===")

EMACS_BATCH = emacs -batch -Q $(LOAD_PATH)

ELS = org-ref-prettify.el
ELCS = $(ELS:.el=.elc)

all: $(ELCS)

%.elc: %.el
	@printf "Compiling $<\n"
	@$(EMACS_BATCH) --eval "\
	  (when (file-exists-p \"$@\")\
	    (delete-file \"$@\"))" \
	-f batch-byte-compile $<

clean:
	@printf "Removing *.elc...\n"
	@$(RM) $(ELCS)
