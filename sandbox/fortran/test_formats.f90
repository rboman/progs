! test print, write statements
!
!  formats:
!     A : une string
!     / : \n
!     5i4 : 5 entiers de taille 4 caracteres
!     5f10.5: 5 floats de taille 10 caracteres avec 5 decimales

program test_formats

    !real*8 nd_scn(5)  ! (-Wpedantic) Warning: GNU Extension: Nonstandard type declaration REAL*8 at (1)
    !real(8) nd_scn(5) ! OK
    real(kind=8) :: nd_scn(5) ! encore plus "moderne"

    integer :: sz = 2
    character(len=20) :: fmt

    nd_scn(1) = 1.0
    nd_scn(2) = 222222.222222222
    nd_scn(3) = 10000.0   ! < genere des etoiles
    nd_scn(4) = 4.444444444444444
    nd_scn(5) = 5.555555555555555

    print "(A)", 'nd_scn ='
    write (*, 10) (nd_scn(ni), ni=1, 5)

10  format(1x, f26.16)  !< equivalent portable
!10 format(1x,f) !< accepté par uniquement par intel
!                (gfortran: Error: Nonnegative width required in format string)

    print "(/,A)", 'en 1 ligne...'  ! / pour sauter une ligne (/ == \n)

    print "(1x, f26.16)", (nd_scn(ni), ni=1, 5)

    print "(/,A)", 'sur 1 ligne...'  ! / pour sauter une ligne (/ == \n)
    print "(1x, 5f26.16)", (nd_scn(ni), ni=1, 5)

    print "(/,A,I2,A)", 'format variable (sur sz=', sz, ' colonnes)...'  ! "Variable FORMAT expressions"
    write (fmt, *) sz
    print "(1x, "//adjustl(fmt)//"f26.16)", (nd_scn(ni), ni=1, 5)

end program test_formats

