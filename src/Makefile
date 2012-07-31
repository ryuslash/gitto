DESTDIR ?= /usr/local
CFLAGS = $(shell pkg-config --cflags guile-2.0) -fPIC
LDFLAGS = $(shell pkg-config --libs guile-2.0)

objects = gitto-path.o

all: libguile-gitto-path.so

.PHONY: install uninstall

libguile-gitto-path.so: gitto-path.o
	$(CC) $(CFLAGS) -shared -o libguile-gitto-path.so $^

install-gitto: gitto
	install -Dm 755 $^ ${DESTDIR}/bin/$^
install-libguile-gitto-path: libguile-gitto-path.so
	install -Dm 755 $^ \
		$(shell pkg-config guile-2.0 --variable=extensiondir)/$^
install: install-gitto install-libguile-gitto-path

uninstall:
	rm -f ${DESTDIR}/bin/gitto
	rm -f $(shell pkg-config guile-2.0 --variable=extensiondir)/libguile-gitto-path.so
