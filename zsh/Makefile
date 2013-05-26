DESTDIR ?= /usr/local

.PHONY: all install uninstall

all:

install:
	install -Dm 644 _gitto $(DESTDIR)/share/zsh/site-functions/_gitto

uninstall:
	rm -f $(DESTDIR)/share/zsh/site-functions/_gitto
