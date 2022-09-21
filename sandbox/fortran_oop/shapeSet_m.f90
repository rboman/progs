module shapeSet_m

    use shape_m
    use domainptr_m

    implicit none

    type shapeSet
        !type(domainptr), dimension(:), allocatable, private :: dom
        type (domainptr), dimension(:), allocatable :: dom
    contains
        procedure :: alloc, add, getdom
    end type shapeSet

contains

    subroutine alloc(this, dim)
        class(shapeSet), intent(inout) :: this
        integer, intent(in) :: dim
        allocate (this%dom(dim))
    end subroutine alloc

    subroutine add(this, ashape, i)
        class(shapeSet), intent(inout) :: this
        class(shape), pointer, intent(in) :: ashape
        integer, intent(in) :: i
        this%dom(i)%p => ashape
    end subroutine add

    function getdom(this, i)
        class(shapeSet), intent(in) :: this
        integer, intent(in) :: i
        class(shape), pointer :: getdom
        getdom => this%dom(i)%p
    end function getdom

end module shapeSet_m
