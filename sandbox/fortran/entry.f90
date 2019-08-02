program main
    call sub1()
    call sub1entry()
end program main


subroutine sub1()
    write(*,*) 'subroutine call executes this part'
entry sub1entry()
    write(*,*) 'both calls execute this part'
end subroutine sub1

