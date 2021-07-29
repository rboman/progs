!
! test stdin with minGW.
!
! cmake -G "Unix Makefiles" ..
! cmake --build . -- -j 3 && test_stdin.exe < ..\test_stdin.txt
!

program test_stdin

    CHARACTER(len=80) :: string
    CHARACTER(len=80) :: ioerrmsg
    real(8) :: val

    INTEGER :: iostatus
    character tab

    tab = char(9)  ! tabulation
    string = 'nop'

    do while (string /= 'stop')
        print *, '** enter a string, then [enter]  ("stop" to stop)'
        ioerrmsg = ''
        !read (*, '(A)', END=10, IOSTAT=iostatus, iomsg=ioerrmsg) string
        read (*, '(A)', IOSTAT=iostatus, iomsg=ioerrmsg) string
        if (ioerrmsg /= '') then
            print *, tab, 'ioerrmsg = ', ioerrmsg
        end if
        if (iostatus > 0) then
  
            print *, tab, 'cannot read STRING (iostatus = ', iostatus, ')'
            cycle
        else if (iostatus < 0) then
          ! utile si pas de END=10
            print *, tab, 'EOF while reading string (iostatus = ', iostatus, ')'
            exit
        else
            ! read (*, '(A)', END=10) string
            ! read (*, *, END=10) string  !  saute les lignes blanches
            ! print *, '"', string, '"'
            print *, tab, 'string="', string, '"'
        end if

        ! read (string, '(F8.4)', END=10, IOSTAT=iostatus) val
        ioerrmsg = ''
        read (string, fmt=*, IOSTAT=iostatus, iomsg=ioerrmsg) val
        if (ioerrmsg /= '') then
            print *, tab, 'ioerrmsg = ', ioerrmsg
        end if
        ! print *, 'iostatus = ', iostatus
        if (iostatus > 0) then
            print *, tab, 'cannot convert STRING to REAL*8 (iostatus = ', iostatus, ')'
        else if (iostatus < 0) then
            print *, tab, 'EOF while converting string (iostatus = ', iostatus, ')'  ! si la chaine est vide!
        else
            ! read (*, '(A)', END=10) string
            ! read (*, *, END=10) string  !  saute les lignes blanches
            ! print *, '"', string, '"'
            print *, tab, 'val = |', val, '|'
        end if
    end do
10  continue
end
