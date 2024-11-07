module io_module
    contains
        subroutine print_message(message)
            character(len=*), intent(in) :: message
    
            ! Affiche le message complet
            print *, trim(message)
        end subroutine print_message
end module io_module
