module string_utils
    contains
        function int_to_string(num, format) result(str)
            integer, intent(in) :: num
            character(len=*), intent(in) :: format
            character(len=32) :: str
    
            ! Convert integer to string with the specified format
            write(str, format) num
    
        end function int_to_string
    
        function real_to_string(num, format) result(str)
            real, intent(in) :: num
            character(len=*), intent(in) :: format
            character(len=32) :: str
    
            ! Convert real to string with the specified format
            write(str, format) num

        end function real_to_string
    end module string_utils
    