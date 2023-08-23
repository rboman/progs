module rectangle_m

    use shape_m
    implicit none
    private
    public rectangle

    type, extends(shape) :: rectangle
    contains
        procedure :: area
    end type rectangle
contains
    function area(this)
        class(rectangle), intent(in) :: this
        real :: area
        area = this%a*this%b
    end function area

end module rectangle_m
