module utilities

contains

    subroutine show_coordinates(nodes_c, nnodes) bind(C, name="show_coordinates")

        use, intrinsic :: iso_c_binding, only: c_int, c_double, c_ptr, C_F_POINTER

        implicit none

        ! Define a C-like structure (defined in 'structure.h') in a Fortran way.
        type, bind(c) :: node
            real(kind=c_double) :: x
            real(kind=c_double) :: y
            integer(kind=c_int) :: tag
        end type

        type(c_ptr), value :: nodes_c
        integer(c_int), value :: nnodes

        type(node), pointer :: nodes(:)
        integer :: i

        call c_f_pointer(nodes_c, nodes, [nnodes])

        print *, "Printing data from Fortran"
        do i = 1, nnodes
            print '("Point ", I2, ":", F12.4, 1x, F12.4) ', nodes(i)%tag, nodes(i)%x, nodes(i)%y
        end do

    end subroutine

end module
