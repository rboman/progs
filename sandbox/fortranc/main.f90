program fortranc
    implicit none

    integer :: i
    double precision :: d

    print *, 'calling C++ routine from FORTRAN...'

    i = 1
    d = 3.14d0

    call croutine(i, d)

    print *, 'back to FORTRAN.'

end
