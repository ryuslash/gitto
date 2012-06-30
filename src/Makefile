DESTDIR ?= /usr/local

all:

.PHONY: install uninstall

install: gitto
	install -Dm 755 $^ ${DESTDIR}/bin/$^

uninstall: gitto
	rm -f ${DESTDIR}/bin/$^
