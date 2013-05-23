DESTDIR ?= /usr/local
CFLAGS = $(shell pkg-config --cflags guile-2.0) -fPIC
LDFLAGS = $(shell pkg-config --libs guile-2.0)
EXTDIR = $(shell pkg-config guile-2.0 --variable=extensiondir \
	--define-variable=prefix=$(DESTDIR))

objects = gitto-path.o

all: libguile-gitto-path.so

.PHONY: install uninstall

%.x: %.c
	guile-snarf -o $@ $< $(CFLAGS)

gitto-path.o: gitto-path.x
libguile-gitto-path.so: gitto-path.o
	$(CC) $(CFLAGS) -shared -o libguile-gitto-path.so $^

install-gitto: gitto
	install -Dm 755 $^ ${DESTDIR}/bin/$^
install-libguile-gitto-path: libguile-gitto-path.so
	install -Dm 755 $^ ${EXTDIR}/$^
install: install-gitto install-libguile-gitto-path

uninstall:
	rm -f ${DESTDIR}/bin/gitto
	rm -f ${EXTDIR}/libguile-gitto-path.so
