
program test_debug

    type s_bandecage
        logical :: etatok
        logical :: selection
    end type s_bandecage

    type(s_bandecage)         :: BandeCage

    BandeCage%etatok = .true.
    BandeCage%selection = .false.

    if (BandeCage%etatok .eqv. .true.) then
        print *, "BandeCage%etaok == .true."
    else
        print *, "BandeCage%etaok == .false."
    end if

end program test_debug
