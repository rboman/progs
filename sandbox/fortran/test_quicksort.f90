! quicksort.f -*-f90-*-
! Author: t-nissie
! License: GPLv3
! Gist: https://gist.github.com/t-nissie/479f0f16966925fa29ea
!!
recursive subroutine quicksort(a, first, last)
    implicit none
    integer  a(*), x, t
    integer first, last
    integer i, j

    x = a( (first+last) / 2 )
    i = first
    j = last
    do
    do while (a(i) < x)
        i=i+1
    end do
    do while (x < a(j))
        j=j-1
    end do
    if (i >= j) exit
    t = a(i);  a(i) = a(j);  a(j) = t
    i=i+1
    j=j-1
    end do
    if (first < i-1) call quicksort(a, first, i-1)
    if (j+1 < last)  call quicksort(a, j+1, last)
end subroutine quicksort

program test_quicksort
    integer, target :: x(10)
    data x/1,4,3,8,2,6,9,7,0,5/

    call quicksort(x, 1, 10)
    print *,x
end
