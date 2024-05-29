! from http://calcul.math.cnrs.fr/Documents/Ecoles/2010/f2py-cours.pdf
! Pierre Navaro

!
! f2py does not handle derived types (structs)
! => the TRICK is to let f2py think that the struct is an integer(8)
!    ! default integer= integer(4) do not work on 64 machines!!
!

module mesh
    implicit none
    type :: geometry
        real(8) :: x0, x1, dx ! coordinates of origin and grid size
        integer :: nx ! number of grid points
        real(8), dimension(:), pointer :: xgrid ! coordinates of points
    end type geometry

contains
    subroutine create(geom,x0,nx,dx)
        !f2py integer(8), intent(out) :: geom
        type(geometry), pointer :: geom 
        !type(geometry), allocatable :: geom  ! also works
        real(8), intent(in) :: x0, dx
        integer, intent(in) :: nx
        integer :: i
        allocate(geom)
        print *, associated(geom)
        !print *, allocated(geom)  ! with allocatable
        geom%x0=x0; geom%x1=x0+nx*dx; geom%dx=dx; geom%nx=nx
        allocate(geom%xgrid(nx))
        do i=1,nx
            geom%xgrid(i)=geom%x0+(i-1)*geom%dx
        end do
    end subroutine create

    subroutine view(this)
        !f2py integer(8), intent(in) :: this
        type(geometry),allocatable :: this
        print*, 'nx = ', this%nx
        print*, 'x0 = ', this%x0, ' x1 = ', this%x1
        print*, 'xgrid = ', this%xgrid(:)
    end subroutine view
end module mesh
