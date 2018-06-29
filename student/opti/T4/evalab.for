      SUBROUTINE EVALAB(A,b,x,N)
C
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION A(*),b(N),x(N)
      REAL*8 l
C
      l=360.0
C
      RAC2=DSQRT(2.0D0)
C
C     Calcul de A
C     -----------
      DO i=1,21
         A(i)=0.0D0
      ENDDO
      A(1)  = 2*l * 1.0  / x(1)**3
      A(3)  = 2*l * 1.0  / x(2)**3
      A(6)  = 2*l * RAC2 / x(3)**3
      A(10) = 2*l * 1.0  / x(4)**3
      A(15) = 2*l * RAC2 / x(5)**3
      A(21) = 2*l * RAC2 / x(6)**3
C
C     Calcul de b
C     -----------
      b(1) = 3*l * 1.0  / x(1)**2
      b(2) = 3*l * 1.0  / x(2)**2
      b(3) = 3*l * RAC2 / x(3)**2
      b(4) = 3*l * 1.0  / x(4)**2
      b(5) = 3*l * RAC2 / x(5)**2
      b(6) = 3*l * RAC2 / x(6)**2
C
      RETURN
      END