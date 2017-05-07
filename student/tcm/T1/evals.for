C==================================================================
C Sous routine EVALS : 
C    . Evaluation de la matrice S=C/dt+theta*K
C    . Si IDIAG = 0, Décomposition LU de cette matrice --> s1 = L
C                                                          s2 = U
C    . Si IDIAG = 1, S(2,1) contient C/dt diagonalisé. 
C
C Modifié le 07.04.97
C==================================================================
C
      SUBROUTINE EVALS(a1,a2,A,theta,S,NN,s1,s2,h,N,dt,IDIAG)
C
      IMPLICIT REAL*8(A-H,O-Z)
C
      DIMENSION S(3,NN),s1(NN),s2(NN)
C
      t1 = a2/dt - a1*theta
      t2 = 4.0 * a2/dt + 2.0 * a1 * theta
C
      IF(IDIAG.EQ.0) THEN
         DO i=1,N+1
            S(1,i)=t1
            S(2,i)=t2
            S(3,i)=t1
         ENDDO
         S(3,1)   = 0.0D0
         S(2,1)   = (a1+h*A)*theta + 2.0*a2/dt
         S(2,N+1) = t2/2.0
         S(1,N+1) = S(3,1)
C
C        Décomposition LU de S
C        ---------------------
         s1(1)   = S(2,1)
C
         DO i=2,N+1
            s2(i)   = S(3,i)/s1(i-1)
            s1(i)   = S(2,i)-s2(i)*S(1,i-1)
         ENDDO
      ELSE
         SOMD=N*t2
         SOMT=SOMD+2*N*t1
         DO i=1,N+1
            S(2,i)=t2
         ENDDO
         S(2,1)   = 2.0*a2/dt
         S(2,N+1) = t2/2.0
         DO i=1,N+1
            S(2,i)=S(2,i)*SOMT/SOMD
         ENDDO         
      ENDIF
C
      RETURN
      END