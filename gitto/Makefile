DESTDIR ?= /usr/local

objects = main.go main.scm
install-objects = $(addprefix install-,$(objects))
uninstall-objects = $(addprefix uninstall-,$(objects))

$(filter %.go,$(objects)): %.go: %.scm
	guile-tools compile -o $@ $^

.PHONY: install $(install-objects) uninstall $(uninstall-objects)

install: $(install-objects)
uninstall: $(uninstall-objects)

$(filter %.go,$(install-objects)): install-%:
	install -Dm 644 $* $(DESTDIR)/lib/guile/2.0/ccache/gitto/$*

$(filter %.scm,$(install-objects)): install-%:
	install -Dm 644 $* $(DESTDIR)/share/guile/2.0/gitto/$*

$(filter %.go,$(uninstall-objects)): uninstall-%:
	rm -f $(DESTDIR)/lib/guile/2.0/ccache/gitto/$*

$(filter %.scm,$(uninstall-objects)): uninstall-%:
	rm -f $(DESTDIR)/share/guile/2.0/gitto/$*
