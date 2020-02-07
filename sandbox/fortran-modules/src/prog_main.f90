program prog_main

    use constants   ! provides constants definitions
    use funhandles  ! the funhandles contains the interface defining the signature of getthepower function
    
    implicit none

    integer :: i
    integer :: N
    real :: result

    N = 10 

    do i = 1, N
        result = getheppower( i )

        if ( i == 1 ) then
            write (*, '("La puissance 1ere du nombre pi est: ",f8.2)') result
        else
            write (*, '("La puissance",i3,"ieme du nombre pi est: ",f8.2)') i, result
        end if 

    end do 

end program prog_main
