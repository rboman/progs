! this module defines a file in memory as a linked-list of lines of variable length.

module memfile
    implicit none
    private

    type :: line_t
        character(len=:), allocatable :: line
        type(line_t), pointer :: next => null()
    end type line_t

    type, public :: memfile_t
        character(len=:), allocatable :: name
        type(line_t), pointer, private :: first => null() !< first line
        type(line_t), pointer, private :: last => null()  !< last line
        type(line_t), pointer, private :: read_cursor => null()

        contains
            procedure :: push_back => memfile_push_back
            procedure :: print => memfile_print
            procedure :: count_lines => memfile_count_lines
            procedure :: read_next => memfile_read_next
            procedure :: rewind => memfile_rewind           
    end type memfile_t

contains
    subroutine memfile_push_back(this, line)
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
    end subroutine memfile_push_back


    subroutine memfile_print(this)
        class(memfile_t), intent(in) :: this
        type(line_t), pointer :: current

        print "(A,A,A,I0,A)", 'file: ', this%name, ' : (', this%count_lines(), ' lines)'
        current => this%first
        do while (associated(current))
            print *, '"'//current%line//'"'
            current => current%next
        end do
    end subroutine memfile_print


    function memfile_count_lines(this) result(n)
        class(memfile_t), intent(in) :: this
        type(line_t), pointer :: current
        integer :: n

        current => this%first
        n = 0
        do while (associated(current))
            n = n + 1
            current => current%next
        end do
    end function memfile_count_lines


    subroutine memfile_rewind(this)
        class(memfile_t), intent(inout) :: this
        this%read_cursor => this%first
    end subroutine memfile_rewind


    function memfile_read_next(this, line) result(ok)
        class(memfile_t), intent(inout) :: this
        character(len=:), allocatable, intent(out) :: line
        logical :: ok

        if (associated(this%read_cursor)) then
            line = this%read_cursor%line
            this%read_cursor => this%read_cursor%next
            ok = .true.
        else
            ok = .false.
        end if
    end function memfile_read_next

end module memfile

