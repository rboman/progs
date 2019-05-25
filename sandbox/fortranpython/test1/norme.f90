subroutine norme ( a , b )
    ! Calcule la norme
    real(8) :: a , b
    real(8) :: c
    c = sqrt ( a*a+b*b )
    print *, " la norme de ( " , a , " , " , b , " ) est " , c
end subroutine norme
