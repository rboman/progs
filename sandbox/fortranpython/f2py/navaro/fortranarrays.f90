! from http://calcul.math.cnrs.fr/Documents/Ecoles/2010/f2py-cours.pdf
! Pierre Navaro


! tableaux:
!  - attention: passer le bon type sinon, f2py effectue une copie
!    et travaille sur une variable temporaire (sans rien dire!)
!  - si multidim: specifier "order='F'" pour le tableau numpy!

subroutine move( positions, vitesses, dt, n)
    integer, intent(in) :: n
    real(8), intent(in) :: dt
    real(8), dimension(n,3), intent(in) :: vitesses
    real(8), dimension(n,3) :: positions
    do i = 1, n
        positions(i,:) = positions(i,:) + dt*vitesses(i,:)
    end do
end subroutine move


! creation d'un tableau et retour a python.
! f2py se charge de la gestion de mémoire
subroutine create_array(A, n)
    integer, intent(in) :: n
    real(8), dimension(n,3), intent(out) :: A
    do i = 1, n
        do j = 1, n
            A(i,j) = i+j
        enddo
    enddo
end subroutine create_array

!
! produit matriciel
!

subroutine mult_array(A, B, C, m, n, k)

    real(8), dimension(m,k), intent(in) :: A
    real(8), dimension(k,n), intent(in) :: B
    integer :: m,n,k
    real(8), dimension(m,n), intent(out) :: C
    
    real(8) ::s
    integer :: i,j,l 

    do i=1, m 
        do j = 1, n 
            s = 0.0
            do l = 1, k
                s = s + A(i,l)*B(l,j)
            enddo
            C(i,j) = s
        enddo
    enddo
end subroutine mult_array