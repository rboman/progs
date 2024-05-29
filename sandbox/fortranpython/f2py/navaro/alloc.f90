! from http://calcul.math.cnrs.fr/Documents/Ecoles/2010/f2py-cours.pdf
! Pierre Navaro


module f90module
    implicit none
    real(8), dimension(:), allocatable :: farray
    
contains

    subroutine init( n ) !Allocation du tableau farray
        integer, intent(in) :: n
        allocate(farray(n))
    end subroutine init

    subroutine test_array()
        print*, allocated(farray), size(farray)
    end subroutine test_array
end module f90module
