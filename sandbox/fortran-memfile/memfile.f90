

module memfile
    implicit none

    type :: line_t
        character(len=:), allocatable :: line
        type(line_t), pointer :: next => null()
    end type line_t

    type, public :: memfile_t
        character(len=:), allocatable :: name
        type(line_t), pointer :: first => null()
        type(line_t), pointer :: last => null()

        contains
            procedure :: write => memfile_write
            procedure :: print => memfile_print
            procedure :: nblines => memfile_nblines
    end type memfile_t

contains
    subroutine memfile_write(this, line)
        class(memfile_t), intent(inout) :: this
        character(len=*), intent(in) :: line

        type(line_t), pointer :: new_line

        allocate(new_line)
        new_line%line = line

        if (associated(this%last)) then
            this%last%next => new_line
        else
            this%first => new_line
        end if

        this%last => new_line
    end subroutine memfile_write

    subroutine memfile_print(this)
        class(memfile_t), intent(in) :: this
        type(line_t), pointer :: current

        print "(A,A,A,I0,A)", 'file: ', this%name, ' : (', this%nblines(), ' lines)'

        current => this%first
        do while (associated(current))
            print *, '"'//current%line//'"'
            current => current%next
        end do
    end subroutine memfile_print

    function memfile_nblines(this) result(n)
        class(memfile_t), intent(in) :: this
        type(line_t), pointer :: current
        integer :: n

        current => this%first
        n = 0
        do while (associated(current))
            n = n + 1
            current => current%next
        end do
    end function memfile_nblines

end module memfile

