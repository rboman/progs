! Julien Heremans

subroutine cumsum( numbers_c, num_numbers ) bind(c, name='cumsum')

    use iso_c_binding, only: c_int, c_ptr, c_f_pointer
    
    implicit none

    type(c_ptr), value, intent(in) :: numbers_c
    integer(c_int), intent(in), value :: num_numbers
    integer(c_int), dimension(:), pointer :: numbers_f
    integer :: i
    integer(c_int) :: sum


    call c_f_pointer( numbers_c, numbers_f, [num_numbers] )

    sum = 0
    do i = 1, num_numbers
        sum = sum + numbers_f(i)
        numbers_f(i) = sum
    end do

    
end subroutine
        