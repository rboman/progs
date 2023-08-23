module shape_m

    implicit none

    type, abstract :: shape
        real :: a, b
    contains
        procedure :: init
        procedure :: print => shape_print
        procedure(interface_area), deferred :: area
    end type shape

    abstract interface
        function interface_area(this)
            import :: shape
            class(shape), intent(in) :: this
            real :: interface_area
        end function interface_area
    end interface

contains

    subroutine init(this, mya, myb)
        class(shape), intent(inout) :: this
        real, intent(in) :: mya, myb
        this%a = mya; this%b = myb
    end subroutine init

    subroutine shape_print(this)
        class(shape), intent(in) :: this

        print *, 'a,b', this%a, this%b
        !print *, 'area ', this%area()  ! OK works
    end subroutine

end module shape_m
