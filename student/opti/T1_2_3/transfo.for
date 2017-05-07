C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C     TRANSFO.FOR : Transforme le problème primal -> dual
C
C Dernière modification : 08.02.97           
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
      SUBROUTINE TRANSFO(A,b,C,d,x,A2,b2,x2,N,m,F2)
C      
C     Déclaration des variables
C     -------------------------
      IMPLICIT REAL*8(a-h,o-z)
      DIMENSION A(*),b(*),C(N,m),d(m),A2(*),b2(*),x(*),x2(*)
C
C     Calcul de -l(lamda)
C     -------------------
C
C     -- Termes quadratiques --     
C
      k=0
      F2=0.0D0
      DO i=1,m
         DO j=1,i
            k=k+1
            A2(k)=0.
            ll=1
            IF(i.EQ.j) THEN
               DO l=1,N
                  A2(k)=A2(k)-c(l,j)**2/A(ll)               
                  ll=ll+l+1
               ENDDO
            ELSE
               DO l=1,N
                  A2(k)=A2(k)-c(l,j)*c(l,i)/A(ll)
                  ll=ll+l+1
               ENDDO
            ENDIF
      A2(k)=-A2(k)
         ENDDO
      ENDDO
C
C     -- Termes linéaires --
C
      DO i=1,m
         b2(i)=0.
         ll=1 
         DO l=1,N
            b2(i)=b2(i)+b(l)*c(l,i)/A(ll)
            ll=ll+l+1
         ENDDO
         b2(i)=b2(i)-d(i)
      ENDDO
C
C     Initialisation du vecteur lamda0 à 0.0
C     --------------------------------------
      DO i=1,m
         x2(i)=0.0D0
      ENDDO
C  
      RETURN
      END