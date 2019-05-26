! f2py2 norme.f90 -m vect
!   produit vecmodule.c
!
! f2py2 norme.f90 -m vect -h norme.pyf
!   produit norme.pyf, un fichier de signature
!
! compile direct avec gfortran (=>vect.pyd):
! f2py2 -c norme.f90 -m vect --fcompiler=gnu95


! depends on
!C:\msys64\mingw64\lib\python2.7\site-packages/numpy/f2py\src\fortranobject.c
!C:\msys64\mingw64\lib\python2.7\site-packages/numpy/f2py\src\fortranobject.h

subroutine norme ( a , b )
    ! Calcule la norme
    real(8) :: a , b
    real(8) :: c
    c = sqrt ( a*a+b*b )
    print *, " la norme de ( " , a , " , " , b , " ) est " , c
end subroutine norme
