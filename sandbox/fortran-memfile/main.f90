! this program writes lines into a memory file and prints it.

program main
    use memfile
    implicit none

    type(memfile_t) :: file     !< file in memory
    character(len=100) :: line  !< this is a tmp buffer. 
    integer :: i

    ! assign name to file
    file%name = "test.gnu"

    ! write lines to file in memory
    do i=1, 10
        write(line, '(A,I0)') "line ", i
        call file%write(trim(line))
    end do

    ! print file in memory
    call file%print()

    print *, "done."

end program main

