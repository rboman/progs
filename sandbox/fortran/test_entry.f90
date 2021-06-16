! example of the "entry" statement

program test_entry
    call sub1()
    call sub1entry()
end program test_entry

subroutine sub1()
    write (*, *) 'subroutine call executes this part'
    entry sub1entry()
    write (*, *) 'both calls execute this part'
end subroutine sub1

