C==================================================================
C Sous routine EVALG : 
C    . Evaluation du second membre (C/dt-(1-theta)*K)*un + forces
C
C Modifié le 06.04.97
C==================================================================
C
      SUBROUTINE EVALG(a1,a2,u,A,Tf,theta,NN,g,h,N,dt,IT,ITMAX,
     #                 IDIAG,S)
C
      IMPLICIT REAL*8(A-H,O-Z)
C
      DIMENSION u(NN),g(NN),S(3,NN)
C
      IF(IDIAG.EQ.0)THEN
         t1 = a2/dt + a1 * (1.0-theta)
         t2 = 4.0 * a2/dt - 2.0 * a1 * (1.0-theta)
         DO i=2,N
            g(i)=(u(i-1)+u(i+1))*t1 + u(i)*t2         
         ENDDO
         g(N+1) = u(N)*t1 + u(N+1)* t2/2.0
         g(1)   = u(2)*t1 + u(1)  * t2/2.0
         g(1)   = g(1) - u(1) * h*A*(1.0-theta) + h*A*Tf
C
         IF (IT.EQ.ITMAX) THEN
            g(1) = g(1) - theta * h*A*Tf
         ENDIF
      ELSE
         t1 = a1
         t2 = - 2.0 * a1
         DO i=1,N+1
            g(i)=u(i)*S(2,i)
         ENDDO
         DO i=2,N
            g(i)=g(i) + (u(i-1)+u(i+1))*t1 + u(i)*t2         
         ENDDO
         g(N+1) = g(N+1) + u(N)*t1 + u(N+1)* t2/2.0
         g(1)   = g(1) + u(2)*t1 + u(1)  * t2/2.0
         g(1)   = g(1) - u(1) * h*A + h*A*Tf
      ENDIF
C
      RETURN
      END