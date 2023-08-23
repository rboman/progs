module line_m

    use shape_m
    implicit none
    private
    public line

    type, extends(shape) :: line
    contains
        procedure :: area
    end type line
contains

    function area(this)
        class(line), intent(in) :: this
        real :: area
        area = this%a - this%b
    end function area

end module line_m
