      SUBROUTINE EVALS(a1,a2,a3,theta,S,NN,s1,s2,N,dt,alpha)
C
      IMPLICIT REAL*8(A-H,O-Z)
C
      DIMENSION S(3,NN),s1(NN),s2(NN)
C
      t1 = a2/dt - a1*theta + a3 * theta * (1-alpha)
      t2 = 4.0 * a2/dt + 2.0 * a1 * theta + 2.0 * a3 * alpha
      t3 = a2/dt - a1*theta + a3 * theta * (-alpha-1)
C
      DO i=2,N
         S(1,i)=t1
         S(2,i)=t2
         S(3,i)=t3
      ENDDO
      S(3,2)   = 0.0D0
      S(1,N) = S(3,2)
C
C     Décomposition LU de S
C     ---------------------
      s1(2)   = S(2,2)
C
      DO i=3,N
         s2(i)   = S(3,i)/s1(i-1)
         s1(i)   = S(2,i)-s2(i)*S(1,i-1)
         WRITE(1,*)'S1(',i,')=',S1(i),';'
         WRITE(1,*)'S2(',i,')=',S2(i),';'
      ENDDO
C
      RETURN
      END