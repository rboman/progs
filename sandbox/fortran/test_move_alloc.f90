! see https://www.intel.com/content/www/us/en/docs/fortran-compiler/developer-guide-reference/2024-0/move-alloc.html

program test_print

    implicit none

    ! This program uses MOVE_ALLOC to make an allocated array X bigger and
    ! keep the old values of X in the variable X. Only one copy of the old values
    ! of X is needed.
    
    integer :: I, N = 2
    real, allocatable :: X(:), Y(:)
    allocate (X(N), Y(2*N))           ! Y is twice as big as X
    X = (/(I, I=1, N)/)               ! put "old values" into X
    Y = -1                            ! put different "old values" into Y
    print *, ' allocated of X is ', allocated(X)
    print *, ' allocated of Y is ', allocated(Y)
    print *, ' old X is ', X
    print *, ' old Y is ', Y
    Y(1:N) = X             ! copy all of X into the first half of Y
    !                        this is the only copying of values required
    print *, ' new Y is ', Y
    call move_alloc(Y, X)  ! X is now twice as big as it was, Y is
    !                        deallocated, the values were not copied from Y to X
    print *, ' allocated of X is ', allocated(X)
    print *, ' allocated of Y is ', allocated(Y)
    print *, ' new X is ', X
end

