export DESTDIR ?= /usr

dirs = src gitto
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
