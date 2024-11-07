! Routine dans un module sans gestion du nom exporté par bind(c)
!
! le nom exporté sera:
! - "__module1_MOD_addition_in_mod" si gfortran
! - "MODULE1_mp_ADDITION_IN_MOD" si ifort
!
! note: gfortran ignore "!dir$ attributes dllexport" et exporte tous les 
!       symboles, même sous windows!

module module1
    implicit none
contains
    real(8) function addition_in_mod(a, b)
        !dir$ attributes dllexport :: addition_in_mod
        real(8), value :: a, b
        addition_in_mod = a + b
    end function addition_in_mod
end module module1
