program main
    use module1
    use module2
    implicit none

    ! Interface copiée/collée de la routine nue "addition"
    interface
        function addition(a, b)
            real(8), value :: a, b
            real(8) :: addition
        end function addition
    end interface

    real(8) :: a, b, result
    a = 5.0
    b = 3.0

    ! Appel des fonctions:
    result = addition(a, b)
    print *, "Le resultat de l'addition (routine nue) est :", result

    result = addition_in_mod(a, b)
    print *, "Le resultat de l'addition (dans un module) est :", result

    result = addition_bindc(a, b)
    print *, "Le resultat de l'addition (bindc) est :", result

end program main
