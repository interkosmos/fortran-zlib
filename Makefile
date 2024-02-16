.POSIX:
.SUFFIXES:

FC      = gfortran
AR      = ar
PREFIX  = /usr

DEBUG   = -g -O0 -Wall -fmax-errors=1
RELEASE = -O2 -march=native

FFLAGS  = $(RELEASE)
LDLAGS  = -I$(PREFIX)/include -L$(PREFIX)/lib
LDLIBS  = -lz
ARFLAGS = rcs
TARGET  = ./libfortran-zlib.a
SHARED  = ./libfortran-zlib.so

.PHONY: all clean shared test test_shared

all: $(TARGET)
shared: $(SHARED)

$(TARGET): src/zlib.f90
	$(FC) $(FFLAGS) -c src/zlib.f90
	$(AR) $(ARFLAGS) $(TARGET) zlib.o

$(SHARED): src/zlib.f90
	$(FC) $(FFLAGS) -fPIC -shared -o $(SHARED) src/zlib.f90 $(LDLIBS)

test: $(TARGET) test/test_zlib.f90
	$(FC) $(FFLAGS) $(LDFLAGS) -o test_zlib test/test_zlib.f90 $(TARGET) $(LDLIBS)

test_shared: $(SHARED) test/test_zlib.f90
	$(FC) $(FFLAGS) $(LDFLAGS) -o test_zlib_shared test/test_zlib.f90 $(SHARED) $(LDLIBS)

clean:
	if [ `ls -1 *.mod 2>/dev/null | wc -l` -gt 0 ]; then rm *.mod; fi
	if [ `ls -1 *.o 2>/dev/null | wc -l` -gt 0 ]; then rm *.o; fi
	if [ -e $(TARGET) ]; then rm $(TARGET); fi
	if [ -e $(SHARED) ]; then rm $(SHARED); fi
	if [ -e test_zlib ]; then rm test_zlib; fi
	if [ -e test_zlib_shared ]; then rm test_zlib_shared; fi
