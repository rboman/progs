
program test_debug

    implicit none

    ! définition d'une structure "s_bandecage"
    type s_bandecage
        logical :: etatok
        logical :: selection
    end type s_bandecage

    ! instanciation de la structure "s_bandecage"
    type(s_bandecage)         :: b

    b%etatok = .true.
    b%selection = .false.

    if (b%etatok .eqv. .true.) then
        print *, "b%etaok == .true."
    else
        print *, "b%etaok == .false."
    end if

end program test_debug
