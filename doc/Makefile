DESTDIR ?= /usr/local
INSTALL_INFO = install-info
TEXI2PDF = texi2pdf
DVIPS = dvips

.PHONY: all html dvi pdf ps install uninstall install-dvi install-pdf \
	install-ps

all: gitto.info

install: gitto.info
	$(INSTALL_DATA) gitto.info "$(DESTDIR)/share/info/gitto.info"
	$(INSTALL_INFO) --dir-file="$(DESTDIR)/share/info/dir" \
		"$(DESTDIR)/share/info/gitto.info"

uninstall:
	$(INSTALL_INFO) --dir-file="$(DESTDIR)/share/info/dir" \
		--delete "$(DESTDIR)/share/info/gitto.info"
	rm -f "$(DESTDIR)/share/info/gitto.info"
	rm -f "$(DESTDIR)/share/doc/gitto/dvi/gitto.dvi"
	rm -f "$(DESTDIR)/share/doc/gitto/pdf/gitto.pdf"
	rm -f "$(DESTDIR)/share/doc/gitto/ps/gitto.ps"
	rm -rf "$(DESTDIR)/share/doc/gitto/html/"

install-dvi: gitto.dvi
	$(INSTALL_DATA) gitto.dvi \
		"$(DESTDIR)/share/doc/gitto/dvi/gitto.dvi"

install-pdf: gitto.pdf
	$(INSTALL_DATA) gitto.pdf \
		"$(DESTDIR)/share/doc/gitto/pdf/gitto.pdf"

install-ps: gitto.ps
	$(INSTALL_DATA) gitto.ps \
		"$(DESTDIR)/share/doc/gitto/ps/gitto.ps"

gitto.info: gitto.texi

html: gitto.texi
	$(MAKEINFO) --html gitto.texi -o html

dvi: gitto.dvi
pdf: gitto.pdf
ps: gitto.ps

gitto.dvi: gitto.texi
	$(TEXI2DVI) gitto.texi

gitto.pdf: gitto.texi
	$(TEXI2PDF) gitto.texi

gitto.ps: gitto.dvi
	$(DVIPS) gitto.dvi
