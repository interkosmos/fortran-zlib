# fortran-zlib
A collection of Fortran 2018 ISO_C_BINDING interfaces to selected zlib
functions.

## Build Instructions
Simply run the provided Makefile:

```
$ make
```

This outputs the static library `libfortran-zlib.a`. Link your program against
`libfortran-zlib.a -lz`. Optionally, overwrite the default compiler:

```
$ make FC=ifort
```

To build the test program, run:

```
$ make test
$ ./test_zlib
```

Alternatively, you can compile the library with *fpm*:

```
$ fpm build --profile=release
```

## Coverage
| C function     | Fortran interface |
|----------------|-------------------|
| `deflate`      | `deflate`         |
| `deflateEnd`   | `deflate_end`     |
| `deflateInit`  | `deflate_init`    |
| `deflateInit2` | `deflate_init2`   |
| `inflate`      | `inflate`         |
| `inflateEnd`   | `inflate_end`     |
| `inflateInit`  | `inflate_init`    |
| `inflateInit2` | `inflate_init2`   |

## Licence
ISC
