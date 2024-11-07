! fichier addition.f90
!module addition_module
!    implicit none
!contains
    real(8) function addition(a, b) !bind(C, name="addition")
        !dir$ attributes dllexport :: addition
        !use iso_c_binding
        real(8), value :: a, b
        addition = a + b
    end function addition
!end module addition_module
