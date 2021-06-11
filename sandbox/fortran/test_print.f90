

program test_print

    implicit none
    integer ctab
    parameter(ctab=10)
    character*1 cflag(ctab)
    real*8 err(ctab)
    integer i, j 

    do i = 1, ctab
        cflag(i) = '*'
        err(i) = 3.14d0
    enddo

    write(*,*) "test print:"
    write (*, 1009) (err(j), cflag(j), j=1, ctab)
1009 format(10(e14.7, a1, 1 x))
! 0.3140000E+01*  0.3140000E+01*  0.3140000E+01*  0.3140000E+01*  0.3140000E+01*  0.3140000E+01*  0.3140000E+01*  0.3140000E+01*  0.3140000E+01*  0.3140000E+01*

end
