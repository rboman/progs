! sort using C qsort
program test_sort
    use, intrinsic:: iso_C_binding
    integer, target :: x(10)
    data x/1,4,3,8,2,6,9,7,0,5/
    interface
        subroutine qsort(array,n,m,compar) bind(c)
            use, intrinsic:: iso_C_binding
            type(c_ptr), value :: array
            integer(c_size_t) ,value:: m, n
            type(c_funptr), value :: compar
        end subroutine
        integer(c_int) function icmp(a,b) bind(c)
            use, intrinsic:: iso_C_binding
            integer(c_int) :: a,b
        end function
    end interface

    call qsort(c_loc(x), 10_c_size_t , c_sizeof(x(1)), c_funloc(icmp))
    print *,x
end
!
integer(c_int) function icmp(a,b) bind(C)
    use, intrinsic:: iso_C_binding
    integer(c_int) :: a,b
    if(a.gt.b) then
        icmp=1
    else if(a.lt.b) then
        icmp=-1
    else
        icmp=0
    endif
    return
end