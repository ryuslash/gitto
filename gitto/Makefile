DESTDIR ?= /usr/local
SITEDIR = $(shell pkg-config guile-2.0 --variable=sitedir --define-variable=prefix=$(DESTDIR))
COMPDIR = $(DESTDIR)/lib/guile/2.0/site-ccache

objects = main.scm main.go
install-objects = $(addprefix install-,$(objects))
uninstall-objects = $(addprefix uninstall-,$(objects))

$(filter %.go,$(objects)): %.go: %.scm
	guile-tools compile -o $@ $^

.PHONY: install $(install-objects) uninstall $(uninstall-objects)

install: $(install-objects)
uninstall: $(uninstall-objects)

$(filter %.go,$(install-objects)): install-%:
	install -Dm 644 $* "$(COMPDIR)/gitto/$*"

$(filter %.scm,$(install-objects)): install-%:
	install -Dm 644 $* "$(SITEDIR)/gitto/$*"

$(filter %.go,$(uninstall-objects)): uninstall-%:
	rm -f "$(COMPDIR)/gitto/$*"

$(filter %.scm,$(uninstall-objects)): uninstall-%:
	rm -f "$(SITEDIR}/gitto/$*"
