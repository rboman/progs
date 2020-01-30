! returns the given entire of pi
function getheppower( power )

    use constants ! the module contains several constants definition, including pi.

    integer, intent(in) :: power 
    real :: getheppower 

    getheppower = pi ** power
    
end function
