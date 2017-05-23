      SUBROUTINE POIDS(X,N,W)
      IMPLICIT REAL*8(A-H,O-Z)
      REAL*8 l
      DIMENSION X(N)

      l=360.0D0
      un=1.0D0
      rho=0.1D0

      W=l*(un/X(1)+un/X(2)+un/X(4))
      W=W+l*DSQRT(2.0D0)*(un/X(3)+un/X(5)+un/X(6))
      W=W*rho

      WRITE(*,*)'Poids actuel = ',W,' lbs'

      RETURN
      END