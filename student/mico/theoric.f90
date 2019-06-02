!C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!C     Projet MICO 1995 - Partie theorique (ver. FORTRAN/Python)
!C
!C            Calcule u(v) et P(u) par Newton Raphson
!C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

module pars

    integer, parameter :: DP  = KIND(1.0D0)   !< double precision

    real(DP) :: h   = 40.     ! height
    real(DP) :: a   = 24. 
    real(DP) :: b   = 40. 
    real(DP) :: Vh  = 160. 
    real(DP) :: Vv  = 200. 
    real(DP) :: E   = 70000.  !  Young's modulus

end module pars    


subroutine theoric(x, y, P, ul, Pl)
    use pars    
    implicit none

    real(DP), intent(inout) :: P(101), x(101), y(101)    !< nonlinear response  
    real(DP), intent(inout) :: Pl(16), ul(16)            !< linear response
    
    ! ----Declarations--------
    integer :: i, j
    logical :: boucle
    integer :: vz
    real(DP) :: Lh, Lv
    real(DP) :: TOL
    real(DP) :: u, v, lastu, F, DF, Du

    ! ----Donnees-------------     

    print *, 'computing solution from theory...'

    Lh  = 2*a                       ! horizontal length
    Lv  = sqrt(h**2+(b+a)**2)       ! vertical length
    TOL = 1e-5                      ! N-R tolerance

    ! ----Trace u fct. de v---
    i = 0 
    u = 0. 
    lastu = 0.

    do j = 0, 90
        v = real(j, KIND(v))
        
        lastu = 2*TOL
        boucle = .true.
        do while(boucle)
            F = force(u,v)
            if (abs(u-lastu)>TOL) then
                DF = dforce(u,v)
                Du = -F/DF 
                lastu = u 
                u = u + Du
            else
                boucle = .false.
            endif
        enddo

        ! fill arrays
        i = i + 1
        x(i) = v
        y(i) = u
        P(i) = (E*Vv/Lv**4)*(v*v+u*u-2*h*v+2*u*(a+b))*(v-h)
    enddo

    ! ----Réponse linéaire----

    do vz = 0, 15
        ul(vz+1) = 2*h*vz*(a+b)*Vv/Lv**4/(Vh/a**2+2*Vv*(a+b)**2/Lv**4)
        Pl(vz+1) = -2*h*Vv*E/Lv**4*(ul(vz+1)*(a+b)-h*vz)
    enddo 

contains
    function force(u, v)
        implicit none
        real(DP) :: u, v
        real(DP) :: force
        force = (Vv/Lv**4) * (v**2+u**2 - 2*h*v + 2*u*(a+b))*(u+a+b)  &
                + (8*Vh/Lh**4)*(u**2+2*a*u)*(u+a)
    end function

    function dforce(u, v)
        implicit none
        real(DP) :: u, v
        real(DP) :: dforce
        dforce = (Vv/Lv**4) * ((u+a+b)*(2*u+2*(a+b))              &
                + (v**2 + u**2 - 2*h*v + 2*u*(a+b)))              &
                + (8*Vh/Lh**4) * ((2*u+2*a)*(u+a)+(u**2+2*u*a))
    end function

end subroutine theoric
