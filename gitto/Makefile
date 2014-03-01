DESTDIR ?= /usr/local
SITEDIR = $(shell pkg-config guile-2.0 --variable=sitedir \
	--define-variable=prefix=$(DESTDIR))
COMPDIR = $(DESTDIR)/lib/guile/2.0/site-ccache

objects = command.scm command.go config.scm config.go git.scm git.go \
	ui.scm ui.go path.scm path.go commands/help.scm commands/help.go \
	main.scm main.go
install-objects = $(addprefix install-,$(objects))
uninstall-objects = $(addprefix uninstall-,$(objects))

warnings = unused-variable unused-toplevel unbound-variable \
	arity-mismatch duplicate-case-datum bad-case-datum format

.PHONY: all install $(install-objects) uninstall $(uninstall-objects) \
	clean

all: $(filter %.go,$(objects))

$(filter %.go,$(objects)): %.go: %.scm
	../env guild compile $(addprefix -W ,$(warnings)) -o $@ $^

install: $(install-objects)
uninstall: $(uninstall-objects)

$(filter %.go,$(install-objects)): install-%: $(filter %.go,$(objects))
	$(INSTALL_DATA) $* "$(COMPDIR)/gitto/$*"

$(filter %.scm,$(install-objects)): install-%:
	$(INSTALL_DATA) $* "$(SITEDIR)/gitto/$*"

$(filter %.go,$(uninstall-objects)): uninstall-%:
	rm -f "$(COMPDIR)/gitto/$*"

$(filter %.scm,$(uninstall-objects)): uninstall-%:
	rm -f "$(SITEDIR)/gitto/$*"

clean:
	rm -f *.go
