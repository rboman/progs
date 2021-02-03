!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!     Projet MICO 1995 - Partie theorique (ver. FORTRAN/Python)
!
!            Calcule u(v) et P(u) par Newton Raphson
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

!                       | P
!                       | 
!                       |  a 
!                       |-->  
!        ^           o------o---u
!        |            \    /|
!        |             \  /  v
!        |              \/
!        |              /\ 
!        |             /  \ 
!        |            /    \ 
!      h |           /      \ 
!        |          /        \ 
!        |         /          \ 
!        |        /            \ 
!        |       /              \ 
!        |      /                \ 
!        v     o         |--------o
!                            b


module pars

    integer, parameter :: DP = KIND(1.0D0)   !< double precision

    real(DP) :: h = 40.     ! height
    real(DP) :: a = 24.     ! x pos of upper join
    real(DP) :: b = 40.     ! x pos of lower join
    real(DP) :: Vh = 160.   ! volume of horizontal rod
    real(DP) :: Vv = 200.   ! volume of vertical rods
    real(DP) :: E = 70000.  ! Young's modulus

    integer, parameter :: ivmax = 91
    integer, parameter :: ivmaxl = 16

end module pars

subroutine theoric(x, y, P, vl, ul, Pl)
    use pars
    implicit none

    real(DP), intent(inout) :: P(ivmax), x(ivmax), y(ivmax)    !< nonlinear response
    real(DP), intent(inout) :: Pl(ivmaxl), vl(ivmaxl), ul(ivmaxl)  !< linear response

    ! ----Declarations--------
    integer :: j
    logical :: converged
    real(DP) :: Lh, Lv
    real(DP) :: TOL
    real(DP) :: u, v, lastu, F, DF, Du

    ! ----Donnees-------------

    print *, 'computing solution from theory...'

    Lh = 2*a                       ! length of the horizontal rod
    Lv = sqrt(h**2 + (b + a)**2)   ! length of the vertical rods
    TOL = 1e-5                     ! N-R tolerance

    ! Nonlinear response
    u = 0.
    lastu = 0.

    do j = 0, ivmax-1
        ! set v
        v = real(j, KIND(v))

        ! solve u = u(v) with a N-R procedure
        lastu = 2*TOL
        converged = .false.
        do while (.not. converged)
            F = force(u, v)
            if (abs(u - lastu) > TOL) then
                DF = dforce(u, v)
                Du = -F/DF
                lastu = u
                u = u + Du
            else
                converged = .true.
            endif
        enddo

        ! compute P from u, v and fill arrays
        x(j+1) = v
        y(j+1) = u
        P(j+1) = (E*Vv/Lv**4)*(v*v + u*u - 2*h*v + 2*u*(a + b))*(v - h)
    enddo

    ! Linear response
    do j = 0, ivmaxl-1
        v = real(j, KIND(v))
        vl(j + 1) = v
        ul(j + 1) = 2*h*v*(a + b)*Vv/Lv**4/(Vh/a**2 + 2*Vv*(a + b)**2/Lv**4)
        Pl(j + 1) = -2*h*Vv*E/Lv**4*(ul(v + 1)*(a + b) - h*v)
    enddo

contains
    function force(u, v)
        implicit none
        real(DP), intent(in) :: u, v
        real(DP) :: force
        force = (Vv/Lv**4)*(v**2 + u**2 - 2*h*v + 2*u*(a + b))*(u + a + b) &
                + (8*Vh/Lh**4)*(u**2 + 2*a*u)*(u + a)
    end function

    function dforce(u, v)
        implicit none
        real(DP), intent(in) :: u, v
        real(DP) :: dforce
        dforce = (Vv/Lv**4)*((u + a + b)*(2*u + 2*(a + b)) &
                             + (v**2 + u**2 - 2*h*v + 2*u*(a + b))) &
                 + (8*Vh/Lh**4)*((2*u + 2*a)*(u + a) + (u**2 + 2*u*a))
    end function

end subroutine theoric
