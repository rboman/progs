
program test_stdin

    INTEGER :: i
    CHARACTER(len=100) :: arg

    DO i = 0, iargc()
        CALL getarg(i, arg)
        print *, 'arg', i, ' = "', arg, '"'
    END DO
end
