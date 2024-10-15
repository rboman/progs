! program "main" modifié par Marc pour tester memfile_list

program main2
    use memfile
    use memfile_list
    implicit none

    type(memfile_t) :: file1, file2, file3, file4     !< files in memory
    character(len=100) :: line_written         !< this is a tmp (write) buffer. 
    integer :: i
    logical :: ok
    character(len=:), allocatable :: line_read !< this is a tmp (read) buffer.

    type(memfile_list_t) :: list, copy_list    !< list of files
    character(len=20) :: name_A, name_B
    type(memfile_t), pointer :: pt_A, pt_B
    character(len=:), allocatable :: line_read_A, line_read_B !< tmp (read) buffers 


    ! assign a name to the memory files
    file1%name = "test1.gnu"
    file2%name = "test2.gnu"
    file3%name = "test3.gnu"
    file4%name = "test4.gnu"

    ! write some lines to the files in memory
    do i=1, 10
        write(line_written, '(A,I0)') "line ", i
        call file1%push_back(trim(line_written))
        write(line_written, '(A,I0)') "line ", i+10
        call file2%push_back(trim(line_written))
        write(line_written, '(A,I0)') "line ", i+20
        call file3%push_back(trim(line_written))
        write(line_written, '(A,I0)') "line ", i+30
        call file4%push_back(trim(line_written))
    end do

    ! ask the file to print its lines to terminal
    call file1%print()
    call file2%print()
    call file3%print()
    call file4%print()

    ! reads the files sequentially from memory
    print '(A)', "sequential read:"
    call file1%rewind()
    call file2%rewind()
    call file3%rewind()
    call file4%rewind()
    do
        ok = file1%read_next(line_read)
        if (.not. ok) exit
        print *, "read: ", '"'//line_read//'"'
    end do

    ! accessing files from list
    ! add files to list
    list%name = "mylist"
    call list%push_back(file1)
    call list%push_back(file2)
    call list%push_back(file3)
    call list%push_back(file4)

    ! number of files stored in list
    print *
    print *, "list of files"
    print "(I0, 3A)", list%count_files(), ' files are stored in "', list%name, '"' 

    ! search for specific files in list
    name_A = "test3.gnu"
    name_B = "test2.gnu"

    pt_A => list%search(name_A)
    call pt_A%rewind()

    pt_B => list%search(name_B)
    call pt_B%rewind()

    ! read these files sequentially from memory
    ! (assumption: same number of lines, just for testing ...)
    do
        ok = pt_A%read_next(line_read_A)
        ok = pt_B%read_next(line_read_B)
        if (.not. ok) exit
        print *, "read: ", '"'//line_read_A//'"'
        print *, "read: ", '"'//line_read_B//'"'
    end do

    ! print files
    print *
    call pt_A%print()

    ! copy file list
    copy_list = list
    copy_list%name = "mycopylist"

    print *
    print *, "list copy"
    print "(I0, 3A)", copy_list%count_files(), ' files are stored in "', &
        copy_list%name, '"' 

    name_A = "test4.gnu"
    name_B = "test1.gnu"

    pt_A => copy_list%search(name_A)
    call pt_A%rewind()

    pt_B => copy_list%search(name_B)
    call pt_B%rewind()

    ! read the file sequentially from memory
    ! (assumption: same number of lines, just for testing ...)
    do
        ok = pt_A%read_next(line_read_A)
        ok = pt_B%read_next(line_read_B)
        if (.not. ok) exit
        print *, "read: ", '"'//line_read_A//'"'
        print *, "read: ", '"'//line_read_B//'"'
    end do

    print *, "done."

end program main2

