! Routine nue, hors de tout module, sans gestion du nom exporté par bind(c)
!
! le nom exporté sera:
! - "addition_" si gfortran
! - "ADDITION" si ifort

real(8) function addition(a, b)
    !dir$ attributes dllexport :: addition
    real(8), value :: a, b
    addition = a + b
end function addition
