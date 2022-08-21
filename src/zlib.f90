! zlib.f90
!
! Fortran 2018 interface bindings to zlib.
!
! Author:  Philipp Engel
! Licence: ISC
module zlib
    use, intrinsic :: iso_c_binding
    implicit none (type, external)
    private

    integer, parameter, public :: z_uint  = c_int
    integer, parameter, public :: z_ulong = c_long
    integer, parameter, public :: z_byte  = c_char

    integer(kind=c_int), parameter, public :: Z_NO_FLUSH      = 0
    integer(kind=c_int), parameter, public :: Z_PARTIAL_FLUSH = 1
    integer(kind=c_int), parameter, public :: Z_SYNC_FLUSH    = 2
    integer(kind=c_int), parameter, public :: Z_FULL_FLUSH    = 3
    integer(kind=c_int), parameter, public :: Z_FINISH        = 4
    integer(kind=c_int), parameter, public :: Z_BLOCK         = 5
    integer(kind=c_int), parameter, public :: Z_TREES         = 6

    integer(kind=c_int), parameter, public :: Z_OK            =  0
    integer(kind=c_int), parameter, public :: Z_STREAM_END    =  1
    integer(kind=c_int), parameter, public :: Z_NEED_DICT     =  2
    integer(kind=c_int), parameter, public :: Z_ERRNO         = -1
    integer(kind=c_int), parameter, public :: Z_STREAM_ERROR  = -2
    integer(kind=c_int), parameter, public :: Z_DATA_ERROR    = -3
    integer(kind=c_int), parameter, public :: Z_MEM_ERROR     = -4
    integer(kind=c_int), parameter, public :: Z_BUF_ERROR     = -5
    integer(kind=c_int), parameter, public :: Z_VERSION_ERROR = -6

    integer(kind=c_int), parameter, public :: Z_NO_COMPRESSION      =  0
    integer(kind=c_int), parameter, public :: Z_BEST_SPEED          =  1
    integer(kind=c_int), parameter, public :: Z_BEST_COMPRESSION    =  9
    integer(kind=c_int), parameter, public :: Z_DEFAULT_COMPRESSION = -1

    integer(kind=c_int), parameter, public :: Z_FILTERED         = 1
    integer(kind=c_int), parameter, public :: Z_HUFFMAN_ONLY     = 2
    integer(kind=c_int), parameter, public :: Z_RLE              = 3
    integer(kind=c_int), parameter, public :: Z_FIXED            = 4
    integer(kind=c_int), parameter, public :: Z_DEFAULT_STRATEGY = 0

    integer(kind=c_int), parameter, public :: Z_BINARY  = 0
    integer(kind=c_int), parameter, public :: Z_TEXT    = 1
    integer(kind=c_int), parameter, public :: Z_ASCII   = Z_TEXT
    integer(kind=c_int), parameter, public :: Z_UNKNOWN = 2

    integer(kind=c_int), parameter, public :: Z_DEFLATED = 8

    type, bind(c), public :: z_stream
        type(c_ptr)           :: next_in   = c_null_ptr
        integer(kind=z_uint)  :: avail_in  = 0
        integer(kind=z_ulong) :: total_in  = 0
        type(c_ptr)           :: next_out  = c_null_ptr
        integer(kind=z_uint)  :: avail_out = 0
        integer(kind=z_ulong) :: total_out = 0
        type(c_ptr)           :: msg       = c_null_ptr
        type(c_ptr)           :: state     = c_null_ptr
        type(c_funptr)        :: zalloc    = c_null_funptr
        type(c_funptr)        :: zfree     = c_null_funptr
        type(c_ptr)           :: opaque    = c_null_ptr
        integer(kind=c_int)   :: data_type = 0
        integer(kind=z_ulong) :: adler     = 0
        integer(kind=z_ulong) :: reserved  = 0
    end type z_stream

    public :: deflate
    public :: deflate_end
    public :: deflate_init
    public :: deflate_init2
    public :: inflate
    public :: inflate_end
    public :: inflate_init
    public :: inflate_init2

    interface
        ! int deflate(z_streamp strm, int flush)
        function deflate(strm, flush) bind(c, name='deflate')
            import :: c_int, z_stream
            implicit none
            type(z_stream),      intent(inout)     :: strm
            integer(kind=c_int), intent(in), value :: flush
            integer(kind=c_int)                    :: deflate
        end function deflate

        ! int deflateEnd(z_streamp strm)
        function deflate_end(strm) bind(c, name='deflateEnd')
            import :: c_int, z_stream
            implicit none
            type(z_stream), intent(inout) :: strm
            integer(kind=c_int)           :: deflate_end
        end function deflate_end

        ! int deflateInit_(z_streamp strm, int level, const char *version, int stream_size)
        function deflate_init_(strm, level, version, stream_size) bind(c, name='deflateInit_')
            import :: c_int, c_ptr, z_stream
            implicit none
            type(z_stream),      intent(inout)     :: strm
            integer(kind=c_int), intent(in), value :: level
            type(c_ptr),         intent(in), value :: version
            integer(kind=c_int), intent(in), value :: stream_size
            integer(kind=c_int)                    :: deflate_init_
        end function deflate_init_

        ! int deflateInit2_(z_streamp strm, int  level, int method, int windowBits, int memLevel,
        !                   int strategy, const char *version, int stream_size)
        function deflate_init2_(strm, level, method, window_bits, mem_level, strategy, &
                version, stream_size) bind(c, name='deflateInit2_')
            import :: c_int, c_ptr, z_stream
            implicit none
            type(z_stream),      intent(inout)     :: strm
            integer(kind=c_int), intent(in), value :: level
            integer(kind=c_int), intent(in), value :: method
            integer(kind=c_int), intent(in), value :: window_bits
            integer(kind=c_int), intent(in), value :: mem_level
            integer(kind=c_int), intent(in), value :: strategy
            type(c_ptr),         intent(in), value :: version
            integer(kind=c_int), intent(in), value :: stream_size
            integer(kind=c_int)                    :: deflate_init2_
        end function deflate_init2_

        ! int inflate(z_streamp strm, int flush)
        function inflate(strm, flush) bind(c, name='inflate')
            import :: c_int, z_stream
            implicit none
            type(z_stream),      intent(inout)     :: strm
            integer(kind=c_int), intent(in), value :: flush
            integer(kind=c_int)                    :: inflate
        end function inflate

        ! int inflateEnd(z_streamp strm)
        function inflate_end(strm) bind(c, name='inflateEnd')
            import :: c_int, z_stream
            implicit none
            type(z_stream), intent(inout) :: strm
            integer(kind=c_int)           :: inflate_end
        end function inflate_end

        ! int inflateInit_(z_streamp strm, const char *version, int stream_size)
        function inflate_init_(strm, version, stream_size) bind(c, name='inflateInit_')
            import :: c_int, c_ptr, z_stream
            implicit none
            type(z_stream),      intent(inout)     :: strm
            type(c_ptr),         intent(in), value :: version
            integer(kind=c_int), intent(in), value :: stream_size
            integer(kind=c_int)                    :: inflate_init_
        end function inflate_init_

        ! int inflateInit2_(z_streamp strm, int  windowBits, const char *version, int stream_size)
        function inflate_init2_(strm, window_bits, version, stream_size) bind(c, name='inflateInit2_')
            import :: c_int, c_ptr, z_stream
            implicit none
            type(z_stream),      intent(inout)     :: strm
            integer(kind=c_int), intent(in), value :: window_bits
            type(c_ptr),         intent(in), value :: version
            integer(kind=c_int), intent(in), value :: stream_size
            integer(kind=c_int)                    :: inflate_init2_
        end function inflate_init2_

        function zlib_version_() bind(c, name='zlibVersion')
            import :: c_ptr
            implicit none
            type(c_ptr) :: zlib_version_
        end function zlib_version_
    end interface
contains
    ! int deflateInit(z_streamp strm, int level)
    integer function deflate_init(strm, level) result(rc)
        type(z_stream), intent(inout) :: strm
        integer,        intent(in)    :: level

        rc = deflate_init_(strm, level, zlib_version_(), int(c_sizeof(strm), kind=c_int))
    end function deflate_init

    ! int deflateInit2(z_streamp strm, int level, int method, int windowBits, int memLevel, int strategy)
    integer function deflate_init2(strm, level, method, window_bits, mem_level, strategy) result(rc)
        type(z_stream), intent(inout) :: strm
        integer,        intent(in)    :: level
        integer,        intent(in)    :: method
        integer,        intent(in)    :: window_bits
        integer,        intent(in)    :: mem_level
        integer,        intent(in)    :: strategy

        rc = deflate_init2_(strm, level, method, window_bits, mem_level, &
                            strategy, zlib_version_(), int(c_sizeof(strm), kind=c_int))
    end function deflate_init2

    ! int inflateInit(z_streamp strm)
    integer function inflate_init(strm) result(rc)
        type(z_stream), intent(inout) :: strm

        rc = inflate_init_(strm, zlib_version_(), int(c_sizeof(strm), kind=c_int))
    end function inflate_init

    ! int inflateInit2(z_streamp strm, int  windowBits)
    integer function inflate_init2(strm, window_bits) result(rc)
        type(z_stream), intent(inout) :: strm
        integer,        intent(in)    :: window_bits

        rc = inflate_init2_(strm, window_bits, zlib_version_(), int(c_sizeof(strm), kind=c_int))
    end function inflate_init2
end module zlib
