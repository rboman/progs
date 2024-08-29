! this program writes lines into a memory file and prints it.

program main
    use memfile
    implicit none

    type(memfile_t) :: file     !< file in memory
    character(len=100) :: line_written         !< this is a tmp (write) buffer. 
    integer :: i
    logical :: ok
    character(len=:), allocatable :: line_read !< this is a tmp (read) buffer.

    ! assign a name to the memory file
    file%name = "test.gnu"

    ! write some lines to the file in memory
    do i=1, 10
        write(line_written, '(A,I0)') "line ", i
        call file%push_back(trim(line_written))
    end do

    ! ask the file to print its lines to terminal
    call file%print()

    ! reads the file sequentially from memory
    print '(A)', "sequential read:"
    call file%rewind()
    do
        ok = file%read_next(line_read)
        if (.not. ok) exit
        print *, "read: ", '"'//line_read//'"'
    end do

    print *, "done."


end program main

