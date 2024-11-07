! Routine dans un module avec gestion du nom exporté par bind(c)
!
! le nom exporté sera:
! - "addition_bindc" dans tous les cas

module module2
    implicit none
contains
    real(8) function addition_bindc(a, b) bind(C, name="addition_bindc")
        !dir$ attributes dllexport :: addition_bindc
        use iso_c_binding
        real(8), value :: a, b
        addition_bindc = a + b
    end function addition_bindc
end module module2
