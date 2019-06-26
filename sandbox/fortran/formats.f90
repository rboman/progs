program main

    real*8 nd_scn(5)

    nd_scn(1) = 1.0
    nd_scn(2) = 222222.222222222
    nd_scn(3) = 1000000000.0   ! < genere des etoiles
    nd_scn(4) = 4.444444444444444
    nd_scn(5) = 5.555555555555555
    
4000 format(1x,f26.16)  !< equivalent portable
!4000 format(1x,f) !< accepté par intel


!boman@garfield:~/dev/progs/sandbox/fortran/build$ ./test_formats 
!        1.0000000000000000
!123456789012345678901234567890

    write(*,4000) (nd_scn(ni),ni=1,5)



end program main


