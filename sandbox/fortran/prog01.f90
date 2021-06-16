! sandbox fortran90

program prog01
    use progmod
    implicit none

    double precision :: z = 1.0d0 ! <= 'd' car 'e' c'est un real (float)
    logical          :: flag = .true.  ! ATTENTION une variable initialisee ici est implictement "save" (static)
    real             :: b
    integer          :: i = 2, &   ! on peut mettre des commentaires ici
                        n = 5; print *, 'hello world i=', i ! <= pas depasser la 132e colonne ------------------------------------->

    b = 2; i = 3
    print *, 'hello &
          &world coupe', flag, z

    do i = 1, n
        print *, 'i=', i
    end do

    print '("hello")'           ! ne laisse pas un caractere blanc (1x) en 1ere colonne!
    print '(A)', "hello"        ! mieux - A = string
    print '(A,i5)', "hello i=", i
    print *, 'globvar=', globvar

end
