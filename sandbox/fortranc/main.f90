program fortranc
    implicit none

    print *, 'calling C++ routine from FORTRAN...'

    call croutine()

    print *, 'back to FORTRAN.'

end
