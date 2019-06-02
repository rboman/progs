! from http://calcul.math.cnrs.fr/Documents/Ecoles/2010/f2py-cours.pdf
! Pierre Navaro

! permet d'ecrire "c=norm(a)" avec un vecteur numpy
! - le fait d'avoir mis "intent(out)" definit la valeur retour (c)
! - f2py comprend que "n" est la taille de "a" et definit un arg optionnel
!   qui vaut par defaut "len(a)"
subroutine norme(a, c, n)
    integer :: n
    real(8), dimension(n), intent(in) :: a
    real(8), intent(out) :: c
    real(8) :: sommec
    integer :: i
    sommec = 0
    do i = 1, n
        sommec = sommec + a(i)*a(i)
    end do
    c = sqrt(sommec)
end subroutine norme
