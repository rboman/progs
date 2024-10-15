! This module defines a list of files in memory
! The list is represented as a linked-list of files, each file consisting
! of lines of variable length

module memfile_list

    use memfile

    implicit none
    private

    type :: list_node_t
        type(memfile_t), pointer :: file => null()
        type(list_node_t), pointer :: next => null()
    end type list_node_t

    type, public :: memfile_list_t
        character(len=:), allocatable :: name
        type(list_node_t), pointer, private :: first => null()
        type(list_node_t), pointer, private :: last => null()

        contains
            procedure :: push_back => memfile_list_push_back
            procedure :: count_files => memfile_list_count_files
            procedure :: search => memfile_list_search
    end type memfile_list_t

contains

    subroutine memfile_list_push_back(this, afile)
        class(memfile_list_t), intent(inout) :: this
        type(memfile_t), intent(in), target :: afile

        type(list_node_t), pointer :: new_node

        allocate(new_node)
        new_node%file => afile

        if (associated(this%last)) then
            this%last%next => new_node
        else
            this%first => new_node
        endif

        this%last => new_node
    end subroutine memfile_list_push_back

    function memfile_list_count_files(this) result(n)
        class(memfile_list_t), intent(in) :: this
        type(list_node_t), pointer :: current
        integer :: n

        current => this%first
        n = 0
        do while (associated(current))
            n = n + 1
            current => current%next
        enddo
    end function memfile_list_count_files

    function memfile_list_search(this, file_name) result(pt_file)
        class(memfile_list_t), intent(inout) :: this
        character(len=*), intent(in) :: file_name
        type(memfile_t), pointer :: pt_file
        type(list_node_t), pointer :: current
        
        logical :: found

        current => this%first
        
        found = .false.
        do while (associated(current))
            if (current%file%name == file_name) then
                found = .true.
                exit
            endif
            current => current%next
        enddo

        if (.not.found) then
            print "(6A)", 'filename "', file_name, '"', ' was not found in list "', &
                this%name, '"'
            stop
        endif

        pt_file => current%file

    end function memfile_list_search

end module memfile_list