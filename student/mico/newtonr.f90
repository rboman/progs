!C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!C      Projet MICO - Newton Raphson (ver. FORTRAN 21.03.95)
!C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

subroutine newtonr(x, y, P)

    implicit none

    integer :: i, nite, compt
    logical :: converged
    double precision :: h, a, b, Vh, Vv, E, Lh, Lv
    double precision :: TOL, u, v
    double precision :: lambda
    double precision, intent(inout) :: P(101), x(101), y(101)
    double precision :: R(2), uu(2)
    double precision :: norm, F(2), Kt(2, 2), Du(2), MAT(2)
    double precision :: invKt(2, 2), dtmKt

    h = 40.
    a = 24.
    b = 40.
    Vh = 160.
    Vv = 200.
    E = 70000.
    Lh = 2*a
    Lv = sqrt(h**2 + (b + a)**2)
    TOL = 1e-5
    i = 0
    lambda = 0.
    uu(1) = 0.
    uu(2) = 0.
    R(1) = 9000.  ! maximum force
    R(2) = 0.

    !C ----converged principale----

    do compt = 0, 100
        lambda = compt/100.

        ! Newton-Raphson
        converged = .false.
        nite = 0
        do while (.not. converged)
            u = uu(1)
            v = uu(2)
            nite = nite + 1
            if (nite > 200) then
                converged = .true.
                print *, "WARNING: max nb of iterations reached!"
            end if

            F(1) = (E*Vv/Lv**4)*(v*v + u*u - 2*h*v + 2*u*(a + b))*(v - h)
            F(2) = (Vv/Lv**4)*(v**2 + u**2 - 2*h*v + 2*u*(a + b))*(u + a + b) &
                   + (8*Vh/Lh**4)*(u**2 + 2*a*u)*(u + a)

            if (lambda /= 0) then
                norm = dsqrt((lambda*R(1) - F(1))**2 + (lambda*R(2) - F(2))**2) &
                       /abs(lambda)/dsqrt(R(1)**2 + R(2)**2)
                ! write(*,*)norm
            else
                norm = 0
            end if

            if (norm > TOL) then
                Kt(1, 1) = E*Vv/(Lv**4)*(2*u + 2*(a + b))*(v - h)
                Kt(1, 2) = E*Vv/(Lv**4)*((v**2 + u**2 - 2*v*h + 2*u*(a + b)) &
                                         + (v - h)*(2*v - 2*h))
                Kt(2, 1) = Vv/(Lv**4)*((v**2 + u**2 - 2*h*v + 2*u*(a + b)) &
                                       + (u + a + b)*(2*u + 2*(a + b))) &
                           + 8*Vh/(Lh**4)*((2*u + 2*a)*(u + a) + (u**2 + 2*a*u))
                Kt(2, 2) = Vv/(Lv**4)*(u + a + b)*(2*v - 2*h)

                MAT(1) = lambda*R(1) - F(1)
                MAT(2) = lambda*R(2) - F(2)
                
                dtmKt = Kt(1, 1)*Kt(2, 2) - Kt(1, 2)*Kt(2, 1)
                
                invKt(1, 1) = Kt(2, 2)/dtmKt
                invKt(2, 2) = Kt(1, 1)/dtmKt
                invKt(1, 2) = -1*Kt(1, 2)/dtmKt
                invKt(2, 1) = -1*Kt(2, 1)/dtmKt

                Du(1) = invKt(1, 1)*MAT(1) + invKt(1, 2)*MAT(2)
                Du(2) = invKt(2, 1)*MAT(1) + invKt(2, 2)*MAT(2)

                uu(1) = uu(1) + Du(1)
                uu(2) = uu(2) + Du(2)
            else
                converged = .true.
            end if
        end do

        i = i + 1
        x(i) = uu(2)
        y(i) = uu(1)
        P(i) = lambda*R(1)
    end do

end subroutine newtonr
