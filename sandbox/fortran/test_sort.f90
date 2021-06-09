! sort things
! from https://www.cita.utoronto.ca/~merz/intel_f10b/main_for/mergedProjects/lref_for/source_files/rfsortq.htm

! https://stackoverflow.com/questions/45871490/open-source-version-of-intel-fortran-sort-routines

module m
    use, intrinsic :: iso_c_binding
    implicit none
    private
  
    interface
        ! prototype Fortran de "qsort()" de libc
        subroutine qsort(base, nel, width, compar) bind(c, name='qsort')
            use, intrinsic :: iso_c_binding
            implicit none
            type(*), intent(inout) :: base(*)
            integer(c_size_t), value :: nel
            integer(c_size_t), value :: width
            abstract interface
            function compar_iface(a, b) result(result) bind(c)
                use, intrinsic :: iso_c_binding
                implicit none            
                integer(c_int) result
                type(c_ptr), value :: a, b
            end function
            end interface
            procedure(compar_iface) compar
        end subroutine
    end interface
end module

function less_int4(a, b) result(result) bind(c)
    use, intrinsic :: iso_c_binding
    integer(c_int) result
    type(c_ptr), value :: a, b
    integer(c_int), pointer :: ap, bp
    call c_f_pointer(a, ap)
    call c_f_pointer(b, bp)
    result = int(ap - bp, c_int)
end function

subroutine my_qsort_int4(a, nel)
    use, intrinsic :: iso_c_binding
    integer(c_int), intent(inout) :: a(*)
    integer(4) :: nel
    ! prototype du ptr de fct C
    abstract interface
        function compar_iface(a, b) result(result) bind(c)
            use, intrinsic :: iso_c_binding
            implicit none            
            integer(c_int) result
            type(c_ptr), value :: a, b
        end function
    end interface
    ! déclaration du ptr de fct
    procedure(compar_iface) less_int4

    ! call de qsort (libc)
    call qsort(a, int(nel, c_size_t), c_sizeof(a(1)), less_int4)
end subroutine



program test_sort
    !    Sort a 1-D array
    !

    !USE IFPORT ! pas portable!
    INTEGER(4) array(10)
    INTEGER(4) i
    DATA ARRAY /143, 99, 612, 61, 712, 9112, 6, 555, 2223, 67/
    !    Sort the array
    !Call SORTQQ (LOC(array), 10, SRT$INTEGER4) ! pas portable!

    call my_qsort_int4(ARRAY, 10) ! version portable

    !    Display the sorted array
    DO i = 1, 10
      WRITE (*, 9000) i, array (i)
    9000 FORMAT(1X, ' Array(',I2, '): ', I5)
    END DO

end program test_sort




