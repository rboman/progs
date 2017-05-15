program mkltest
    
    implicit none
    
    integer :: i, n, incx, incy
    double precision ::  a
    double precision, dimension(1:3) :: x
    double precision, dimension(1:3) :: y
    double precision, dimension(1:3) :: res
    
    x(1) = 2.0
    x(2) = 1.0
    x(3) = -1.0

    y(1) = 1.0
    y(2) = 2.0
    y(3) = 3.0

    a = 2.0
    incx = 1
    incy = 1
    n = 3

    do i=1,3
        res(i) = a*x(i)+y(i)
        print *, 'res(',i,')=', res(i)
    end do

    print *, 'calling daxpy...'

    call daxpy(n, a, x, incx, y, incy)

    do i=1,3
        print *, 'y(',i,')=', y(i)
    end do



end program