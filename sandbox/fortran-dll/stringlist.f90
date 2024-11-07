! Routine avec argument n'ayant pas d'équivalent C
!
! le nom exporté sera:
! - "__stringlist_MOD_print_stringlist" si gfortran
! - "STRINGLIST_mp_PRINT_STRINGLIST" si ifort

module stringlist
    implicit none
contains
    subroutine print_stringlist(slist)
        !dir$ attributes dllexport :: print_stringlist
        character (len=:), dimension(:), allocatable, intent(in) :: slist
        integer :: i
        do i = 1, size(slist)
            print *, slist(i)
        end do
    end subroutine print_stringlist
end module stringlist
