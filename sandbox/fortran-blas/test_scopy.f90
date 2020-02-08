program copy_main
    real x(10), y(10)
    integer n, incx, incy, i
    n = 3
    incx = 3
    incy = 1
    do i = 1, 10
      x(i) = i
    end do
    call scopy (n, x, incx, y, incy)
    print *, 'Y = ', (y(i), i = 1, n)
end
