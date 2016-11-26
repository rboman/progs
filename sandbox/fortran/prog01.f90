program prog01
    use progmod
    implicit none

    double precision :: z = 1.0d0 ! <= 'd' car 'e' c'est un real (float) 
    logical          :: flag=.true.
    real             :: b    
    integer          :: i=2, &   ! on peut mettre des commentaires ici
                        n=5 ;    print *,'hello world i=',i ! <= pas depasser la 132e colonne
    
    b=2; i=3
    print *, 'hello &
          &world coupe', flag, z
    
    do i=1,n
        print *,'i=',i
    end do
    
    print '("hello")'
    print '("hello i=",i5)',i
    print *,'globvar=', globvar

end
