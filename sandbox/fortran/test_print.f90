
program test_print

    implicit none

    integer ctab
    parameter(ctab=10)
    ! integer, parameter :: ctab = 10     ! "modern way"     

    character*1 cflag(ctab)     ! un tableau de 10 caractères

    real*8 err(ctab)            ! un tableau de 10 réels

    integer i, j

    do i = 1, ctab
        cflag(i) = '*'
        err(i) = 3.14d0
    end do

    write (*, *) "test print:"
    write (*, 1009) (err(j), cflag(j), j=1, ctab)
1009 format(10(e14.7, a1, 1 x))
    ! output:
    ! 0.3140000E+01*  0.3140000E+01*  0.3140000E+01*  0.3140000E+01*  0.3140000E+01*  0.3140000E+01*  0.3140000E+01*  0.3140000E+01*  0.3140000E+01*  0.3140000E+01*

    print "(1 x, '>> Votre licence de LAM3 ne permet pas la simulation ', 'du laminage des produits plats')"
    print "(1x, '>> Votre licence de LAM3 ne permet pas la simulation du laminage des produits plats')"
    print "('>> Votre licence de LAM3 ne permet pas la simulation du laminage des produits plats')"

end
