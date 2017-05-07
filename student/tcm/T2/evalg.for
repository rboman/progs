      SUBROUTINE EVALG(a1,a2,a3,u,T0,TL,theta,NN,g,N,dt,alpha)
C
      IMPLICIT REAL*8(A-H,O-Z)
C
      DIMENSION u(NN),g(NN)
C
      t1 = a2/dt + a1 * (1.0-theta) - a3 * (1.0-theta) * (1.0-alpha)
      t2 = 4.0 * a2/dt - 2.0 * a1 * (1.0-theta) - 2.0 * a3 * alpha
     *     *(1.0-theta)
      t3 = a2/dt + a1*(1.0-theta) - a3 * (1.0-theta) * (-alpha-1.0)
C
      u(1)=T0
      u(N+1)=TL
      DO i=2,N
         g(i)=u(i-1)*t3 + u(i+1)*t1 + u(i)*t2         
      ENDDO
      t1 = a2/dt - a1*theta + a3 * theta * (1-alpha)
      t3 = a2/dt - a1*theta + a3 * theta * (-alpha-1)
      g(N) = g(N) - TL * t1
      g(2) = g(2) - T0 * t3
c      DO i=2,N
c         WRITE(*,*)g(i),t2,t3
c      ENDDO
c      READ(*,*)
C
      RETURN
      END