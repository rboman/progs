! from http://calcul.math.cnrs.fr/Documents/Ecoles/2010/f2py-cours.pdf
! Pierre Navaro


module mem
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

    subroutine give_array(a) !!!!!!!!!!!!!!!!!!!!!!!!!MARCHE PO
        real(8), dimension(:), allocatable :: a
        allocate(a(100))
    end subroutine give_array 

end module mem
