subroutine lltsol(n, y, x, alu, jlu, ju)

    implicit none
    !implicit real*8(a-h,o-z)

    integer n, jlu(*), ju(*)
    real*8 x(n), y(n), alu(*)

    ! local variables

    integer i, k
    real*8 sum

    ! forward solve (with L)

    do i = 1, n
        sum = 0.0D0
        do k=ju(i),ju(i+1)-1
            sum = sum + alu(k) * x(jlu(k))
        enddo
        x(i) = (y(i) - sum) * alu(i)
    enddo
 
    ! backward solve (with L^T)

    do i = n,1,-1
        do k = ju(i), ju(i+1)-1
            x(jlu(k)) = x(jlu(k)) - alu(k) * x(i) *alu(i)
        enddo
        x(i) = x(i) * alu(i)
    enddo

end subroutine lltsol

