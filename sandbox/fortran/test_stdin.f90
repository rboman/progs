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

    string = 'nop'

    do while (string /= 'stop')
        print *, '** enter a string, then [enter]  ("stop" to stop)'
        read (*, '(A)', END=10, IOSTAT=iostatus, iomsg=ioerrmsg) string
        print *, 'ioerrmsg = ', ioerrmsg
        if (iostatus > 0) then

          print *, 'cannot read STRING (iostatus = ', iostatus,')'
        else if (iostatus < 0) then
          print *, 'EOF (iostatus = ', iostatus,')'
        else
            ! read (*, '(A)', END=10) string
            ! read (*, *, END=10) string  !  saute les lignes blanches
            ! print *, '"', string, '"'
            print *, 'string="', string, '"'
        end if

        ! read (string, '(F8.4)', END=10, IOSTAT=iostatus) val
        read (string, fmt=*, IOSTAT=iostatus, iomsg=ioerrmsg) val
        print *, 'ioerrmsg = ', ioerrmsg
        ! print *, 'iostatus = ', iostatus
        if (iostatus > 0) then
            print *, 'cannot convert STRING to REAL*8 (iostatus = ', iostatus,')' 
        else if (iostatus < 0) then
          print *, 'EOF (iostatus = ', iostatus,')'  ! si la chaine est vide!
        else
            ! read (*, '(A)', END=10) string
            ! read (*, *, END=10) string  !  saute les lignes blanches
            ! print *, '"', string, '"'
            print *, 'val = |', val, '|'
        end if
    end do
10  continue
end
