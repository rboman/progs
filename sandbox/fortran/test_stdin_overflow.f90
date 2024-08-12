!
! test stdin with MinGW.
!
! cmake -G "Unix Makefiles" ..
! cmake --build . -- -j 3 && test_stdin.exe < ..\test_stdin.txt
!

program test_stdin

    CHARACTER(len=10) :: string  ! short string
    CHARACTER(len=80) :: ioerrmsg
    INTEGER :: iostatus
    character tab

    tab = char(9)  ! tabulation
    string = 'nop'

    do while (string /= 'stop')
        print *, '** enter a string, then [enter]  ("stop" to stop)'
        ioerrmsg = ''
        read (*, '(A)', IOSTAT=iostatus, iomsg=ioerrmsg) string
        if (ioerrmsg /= '') then
            print *, tab, 'ioerrmsg = ', ioerrmsg
        end if
        if (iostatus > 0) then
            print *, tab, 'cannot read STRING (iostatus = ', iostatus, ')'
            cycle
        else if (iostatus < 0) then
            print *, tab, 'EOF while reading string (iostatus = ', iostatus, ')'
            exit
        else
            print *, tab, 'string="', string, '"'
        end if
    end do

end program
