INSTALL = install -D

export DESTDIR ?= /usr/local
export INSTALL_PROGRAM = $(INSTALL)
export INSTALL_DATA = $(INSTALL) -m 644

dirs = doc src gitto zsh
install-dirs = $(addprefix install-,$(dirs))
uninstall-dirs = $(addprefix uninstall-,$(dirs))
clean-dirs = $(addprefix clean-,$(dirs))

.PHONY: all $(dirs) install $(install-dirs) uninstall $(uninstall-dirs) \
	clean $(clean-dirs)

all: $(dirs)
install: $(install-dirs)
uninstall: $(uninstall-dirs)
clean: $(clean-dirs)

$(dirs):
	$(MAKE) -C $@/

$(install-dirs): install-%:
	$(MAKE) -C $*/ install

$(uninstall-dirs): uninstall-%:
	$(MAKE) -C $*/ uninstall

$(clean-dirs): clean-%:
	$(MAKE) -C $*/ clean

check:
	./env guile --no-auto-compile tests/command.scm
