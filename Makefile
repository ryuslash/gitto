INSTALL = install -D

export DESTDIR ?= /usr/local
export INSTALL_PROGRAM = $(INSTALL)
export INSTALL_DATA = $(INSTALL) -m 644

dirs = doc src gitto zsh
install-dirs = $(addprefix install-,$(dirs))
uninstall-dirs = $(addprefix uninstall-,$(dirs))

.PHONY: all $(dirs) install $(install-dirs) uninstall $(uninstall-dirs)

all: $(dirs)
install: $(install-dirs)
uninstall: $(uninstall-dirs)

$(dirs):
	$(MAKE) -C $@/

$(install-dirs): install-%:
	$(MAKE) -C $*/ install

$(uninstall-dirs): uninstall-%:
	$(MAKE) -C $*/ uninstall
