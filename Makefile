.POSIX:

FC      = gfortran
AR      = ar
PREFIX  = /usr
FFLAGS  =
LDLAGS  = -I$(PREFIX)/include/ -L$(PREFIX)/lib/
LDLIBS  = -lz
ARFLAGS = rcs
TARGET  = libfortran-zlib.a

.PHONY: all clean test

all: $(TARGET)

$(TARGET):
	$(FC) $(FFLAGS) -c src/zlib.f90
	$(AR) $(ARFLAGS) $(TARGET) zlib.o

test: $(TARGET)
	$(FC) $(FFLAGS) $(LDFLAGS) -o test_zlib test/test_zlib.f90 $(TARGET) $(LDLIBS)

clean:
	rm *.o
	rm *.mod
	rm $(TARGET)
	rm test_zlib
