program main
    use string_utils
    use io_module
    implicit none

    ! Appel direct de la sous-routine avec la chaîne construite en argument
    call print_message("Entier : " //  trim(int_to_string(42, '(I5)')) &
    // ", Reel : " // trim(real_to_string(3.14159, '(F8.2)')) &
    // ", Message : Hello")

end program main