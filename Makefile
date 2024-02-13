.POSIX:
.SUFFIXES:

FC      = gfortran
AR      = ar
PREFIX  = /usr/local

GNU-VER = $(shell  gfortran --version | awk '/GNU/ && ($$4+0)>0{print $$4}')

DEBUG   = -g -O0 -Wall -fmax-errors=1
RELEASE = -O2 -march=native

FFLAGS  = $(RELEASE)
FFLAGS_SHARED = -shared -fPIC
LDFLAGS  = -I$(PREFIX)/include -L$(PREFIX)/lib
LDLIBS  = -lz
ARFLAGS = rcs
TARGET  = libfortran-zlib.a
SHARED_TARGET = libfortran-zlib.so

.PHONY: all clean test

all: $(TARGET) $(SHARED_TARGET)

$(TARGET):
	$(FC) $(FFLAGS) -c src/zlib.f90
	$(AR) $(ARFLAGS) $(TARGET) zlib.o

$(SHARED_TARGET):
	$(FC) $(FFLAGS) $(FFLAGS_SHARED) src/zlib.f90 -o $(SHARED_TARGET)

test: $(TARGET) $(SHARED_TARGET)
	$(FC) $(FFLAGS) $(LDFLAGS) -o test_zlib test/test_zlib.f90 $(TARGET) $(LDLIBS)
	$(FC) $(FFLAGS) $(LDFLAGS) -o shared_test_zlib test/test_zlib.f90 ./libfortran-zlib.so $(LDLIBS)

install: $(TARGET) $(SHARED_TARGET) pkgcfg
	install -d $(DESTDIR)$(PREFIX)/lib
	install -m 644 $(TARGET) $(DESTDIR)$(PREFIX)/lib
	install -m 644 $(SHARED_TARGET) $(DESTDIR)$(PREFIX)/lib
	install -d $(DESTDIR)$(PREFIX)/include/fortran-zlib/GNU-$(GNU-VER)
	install -m 644 zlib.mod $(DESTDIR)$(PREFIX)/include/fortran-zlib/GNU-$(GNU-VER)
	install -d $(DESTDIR)$(PREFIX)/lib/pkgconfig
	install -m 644 fortran-zlib.pc $(DESTDIR)$(PREFIX)/lib/pkgconfig

uninstall:
	rm $(DESTDIR)$(PREFIX)/lib/$(TARGET)
	rm $(DESTDIR)$(PREFIX)/lib/$(SHARED_TARGET)
	rm $(DESTDIR)$(PREFIX)/include/fortran-zlib/GNU-$(GNU-VER)/zlib.mod
	rm $(DESTDIR)$(PREFIX)/lib/pkgconfig/fortran-zlib.pc

clean:
	if [ `ls -1 *.mod 2>/dev/null | wc -l` -gt 0 ]; then rm *.mod; fi
	if [ `ls -1 *.o 2>/dev/null | wc -l` -gt 0 ]; then rm *.o; fi
	if [ -e test_zlib ]; then rm test_zlib; fi
	if [ -e shared_test_zlib ]; then rm shared_test_zlib; fi
	if [ -e $(TARGET) ]; then rm $(TARGET); fi
	if [ -e $(SHARED_TARGET) ]; then rm $(SHARED_TARGET); fi
	if [ -e fortran-zlib.pc ]; then rm fortran-zlib.pc; fi

pkgcfg:
	cp fortran-zlib.pc.proto fortran-zlib.pc
	sed -i 's#PREFIX#$(PREFIX)#' fortran-zlib.pc
	sed -i 's#GNU-VER#$(GNU-VER)#' fortran-zlib.pc
