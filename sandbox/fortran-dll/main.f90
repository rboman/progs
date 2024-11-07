! fichier main.f90
program main
    !use iso_c_binding
    implicit none

    ! Interface pour la fonction de la DLL
    interface
        function addition(a, b) !bind(C, name="addition")
            !use iso_c_binding
            real(8), value :: a, b
            real(8) :: addition
        end function addition
    end interface

    ! Déclaration des variables
    real(8) :: a, b, result

    ! Initialiser les valeurs
    a = 5.0
    b = 3.0

    ! Appeler la fonction de la DLL
    result = addition(a, b)
    print *, "Le resultat de l'addition est :", result

end program main
