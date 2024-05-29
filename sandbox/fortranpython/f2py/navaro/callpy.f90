! from http://calcul.math.cnrs.fr/Documents/Ecoles/2010/f2py-cours.pdf
! Pierre Navaro

! cette fonction appelle une fonction python

subroutine sommef(f, n, s)
    ! Calcule la somme (f(i), i=1,n)
    external f
    integer, intent(in) :: n
    real, intent(out) :: s
    s = 0.0
    do i = 1, n
        s = s + f(i)
    end do
end subroutine sommef
