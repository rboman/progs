program main

    use shape_m
    use line_m
    use rectangle_m
    use shapeSet_m
    use domainptr_m

    implicit none

    type(line), dimension(3), target :: l
    type(rectangle), dimension(3), target :: r

    class(shape), dimension(:), pointer :: lptr
    class(shape), dimension(:), pointer :: rptr

    type(shapeSet) myset

    ! type(domainptr) :: adom
    class(shape), pointer :: tmp_shape

    integer :: i

    ! Initialization
    call l(1)%init(2.0, -3.0)
    call l(2)%init(-9.0, -4.0)
    call l(3)%init(7.0, 6.0)

    call r(1)%init(7.0, -5.0)
    call r(2)%init(1.0, -8.0)
    call r(3)%init(3.0, 9.0)

    lptr => l
    rptr => r

    ! Build myset object
    ! In practice, this object will be used in other procedures
    call myset%alloc(6)
    do i = 1, 3
        call myset%add(lptr(i), i)
    end do
    do i = 1, 3
        call myset%add(rptr(i), i + 3)
    end do

    ! Exploit polymorphism when calling function 'area'

    !do i=1,6
    !    print *, 'area (', i, '): ', myset%dom(i)%p%area()   ! OK if dom is public in shapeSet class
    !enddo

    do i = 1, 6
        tmp_shape => myset%getdom(i)
        ! print *, 'area (', i, '): ', tmp_shape%area()   ! If dom is private: use function 'getdom', but error #8346
        !print *, 'area (', i, '): ', myset%getdom(i)%area()
        !print *, 'area (', i, '): ', adom%p%area()   ! If dom is private: use function 'getdom', but error #8346
        ! A function reference cannot be used as the leftmost
        ! part-ref of structure component
    end do

    ! workaround if dom is private, but not very satisfactory...
    do i = 1, 6
        associate (adom => myset%getdom(i))
            print *, 'area (', i, '): ', adom%area()
        end associate
    end do

end program
