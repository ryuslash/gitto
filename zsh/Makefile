DESTDIR ?= /usr/local

.PHONY: all install uninstall clean

all:

install:
	$(INSTALL_DATA) _gitto $(DESTDIR)/share/zsh/site-functions/_gitto

uninstall:
	rm -f $(DESTDIR)/share/zsh/site-functions/_gitto

clean:
