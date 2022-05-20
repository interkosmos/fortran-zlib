program test_zlib
    use, intrinsic :: iso_c_binding, only: c_loc
    use, intrinsic :: iso_fortran_env, only: i8 => int64
    use :: zlib
    implicit none (type, external)

    integer, parameter :: CHUNK = 16384

    character(len=*), parameter :: SRC_FILE  = 'test/test.txt'
    character(len=*), parameter :: DST_FILE  = 'test.txt.z'
    character(len=*), parameter :: DST_FILE2 = 'test2.txt'

    character(len=*), parameter :: IN = &
        repeat('Now is the time for all good men to come to the aid of the party. ', 10)

    character(len=:), allocatable :: out1, out2
    integer                       :: rc, sz1, sz2, sz3
    logical                       :: exists

    ! Deflate/inflate file.
    inquire (exist=exists, file=SRC_FILE, size=sz1)
    if (.not. exists) stop 'Error: source file not found'

    rc = zip_deflate_file(SRC_FILE, DST_FILE, Z_DEFAULT_COMPRESSION)
    if (rc /= Z_OK) stop 'Error: zip_deflate_file() failed'

    inquire (exist=exists, file=DST_FILE, size=sz2)
    if (.not. exists) stop 'Error: deflated file not found'

    rc = zip_inflate_file(DST_FILE, DST_FILE2)
    if (rc /= Z_OK) stop 'Error: zip_inflate_file() failed'

    inquire (exist=exists, file=DST_FILE2, size=sz3)
    if (.not. exists) stop 'Error: inflated file not found'

    if (sz1 /= sz3) stop 'Error: file sizes do not match'

    print '("input file size..: ", i0)', sz1
    print '("deflate file size: ", i0)', sz2
    print '("inflate file size: ", i0)', sz3

    ! Deflate/inflate memory.
    rc = zip_deflate_mem(IN, out1, Z_DEFAULT_COMPRESSION)
    if (rc /= Z_OK) stop 'Error: zip_deflate_mem() failed'

    rc = zip_inflate_mem(out1, out2, len(in) * 2)
    if (rc /= Z_OK) stop 'Error: zip_inflate_mem() failed'

    if (IN /= out2) stop 'Error: data mismatch'

    print '("source size......: ", i0)', len(in)
    print '("deflate size.....: ", i0)', len(out1)
    print '("inflate size.....: ", i0)', len(out2)
contains
    integer function zip_deflate_file(source, dest, level) result(rc)
        character(len=*), intent(in) :: source
        character(len=*), intent(in) :: dest
        integer,          intent(in) :: level

        character(len=CHUNK), target :: in
        character(len=CHUNK), target :: out

        character      :: byte
        integer        :: err, flush, have
        integer        :: i, n
        integer        :: in_unit, out_unit
        type(z_stream) :: strm

        rc = deflate_init(strm, level)
        if (rc /= Z_OK) return

        def_block: block
            rc = Z_ERRNO

            open (access='stream', action='read', file=source, form='unformatted', &
                  iostat=err, newunit=in_unit, status='old')
            if (err /= 0) exit def_block

            open (access='stream', action='write', file=dest, form='unformatted', &
                  iostat=err, newunit=out_unit, status='replace')
            if (err /= 0) exit def_block

            do
                n = 0
                flush = Z_NO_FLUSH

                do i = 1, CHUNK
                    read (in_unit, iostat=err) byte

                    if (is_iostat_end(stat)) then
                        flush = Z_FINISH
                        exit
                    end if

                    in(i:i) = byte
                    n = n + 1
                end do

                strm%avail_in = n
                strm%next_in = c_loc(in)

                do
                    strm%avail_out = CHUNK
                    strm%next_out = c_loc(out)
                    rc = deflate(strm, flush)
                    if (rc == Z_STREAM_ERROR) exit def_block
                    have = CHUNK - strm%avail_out
                    write (out_unit, iostat=err) out(1:have)
                    if (err /= 0) exit def_block
                    if (strm%avail_out /= 0) exit
                end do

                if (strm%avail_in /= 0) exit def_block
                if (flush == Z_FINISH) exit
            end do

            if (rc /= Z_STREAM_END) exit def_block
            rc = Z_OK
        end block def_block

        err = deflate_end(strm)
        close (out_unit)
        close (in_unit)
    end function zip_deflate_file

    integer function zip_deflate_mem(source, dest, level) result(rc)
        character(len=*), target,      intent(in)  :: source
        character(len=:), allocatable, intent(out) :: dest
        integer,                       intent(in)  :: level

        character(len=len(source)), target :: buffer
        integer                            :: err, have
        type(z_stream)                     :: strm

        dest = ''

        rc = deflate_init(strm, level)
        if (rc /= Z_OK) return

        def_block: block
            strm%total_in = len(source)
            strm%avail_in = len(source)
            strm%next_in = c_loc(source)

            strm%total_out = len(buffer)
            strm%avail_out = len(buffer)
            strm%next_out = c_loc(buffer)

            rc = deflate(strm, Z_FINISH)
            if (rc == Z_STREAM_ERROR) exit def_block
            have = len(buffer) - strm%avail_out
            dest = buffer(1:have)

            if (rc /= Z_STREAM_END) exit def_block
            rc = Z_OK
        end block def_block

        err = deflate_end(strm)
    end function zip_deflate_mem

    integer function zip_inflate_file(source, dest) result(rc)
        character(len=*), intent(in) :: source
        character(len=*), intent(in) :: dest

        character(len=CHUNK), target :: in
        character(len=CHUNK), target :: out

        character      :: byte
        integer        :: err, have
        integer        :: i, n
        integer        :: in_unit, out_unit
        type(z_stream) :: strm

        rc = inflate_init(strm)
        if (rc /= Z_OK) return

        def_block: block
            rc = Z_ERRNO

            open (access='stream', action='read', file=source, form='unformatted', &
                  iostat=err, newunit=in_unit, status='old')
            if (err /= 0) exit def_block

            open (access='stream', action='write', file=dest, form='unformatted', &
                  iostat=err, newunit=out_unit, status='replace')
            if (err /= 0) exit def_block

            do
                n = 0

                do i = 1, CHUNK
                    read (in_unit, iostat=err) byte
                    if (is_iostat_end(stat)) exit

                    in(i:i) = byte
                    n = n + 1
                end do

                strm%avail_in = n
                strm%next_in = c_loc(in)

                do
                    strm%avail_out = CHUNK
                    strm%next_out = c_loc(out)
                    rc = inflate(strm, Z_NO_FLUSH)
                    if (rc == Z_STREAM_ERROR) exit def_block
                    if (rc == Z_NEED_DICT) exit def_block
                    if (rc == Z_DATA_ERROR) exit def_block
                    if (rc == Z_MEM_ERROR) exit def_block
                    have = CHUNK - strm%avail_out
                    write (out_unit, iostat=err) out(1:have)
                    if (err /= 0) exit def_block
                    if (strm%avail_out /= 0) exit
                end do

                if (rc == Z_STREAM_END) exit
            end do

            rc = Z_OK
        end block def_block

        err = inflate_end(strm)
        close (out_unit)
        close (in_unit)
    end function zip_inflate_file

    integer function zip_inflate_mem(source, dest, buffer_size) result(rc)
        character(len=*), target,      intent(in)  :: source
        character(len=:), allocatable, intent(out) :: dest
        integer,                       intent(in)  :: buffer_size

        character(len=buffer_size), target :: buffer
        integer                            :: err, have
        type(z_stream)                     :: strm

        dest = ''

        rc = inflate_init(strm)
        if (rc /= Z_OK) return

        def_block: block
            strm%total_in = len(source)
            strm%avail_in = len(source)
            strm%next_in = c_loc(source)

            strm%total_out = len(buffer)
            strm%avail_out = len(buffer)
            strm%next_out = c_loc(buffer)

            rc = inflate(strm, Z_FINISH)
            if (rc == Z_STREAM_ERROR) exit def_block
            have = len(buffer) - strm%avail_out
            dest = buffer(1:have)

            if (rc /= Z_STREAM_END) exit def_block
            rc = Z_OK
        end block def_block

        err = inflate_end(strm)
    end function zip_inflate_mem
end program test_zlib
