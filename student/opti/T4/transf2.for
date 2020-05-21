C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C     TRANSF2.FOR : Calcul des x en fct. des lamda
C
C Dernière modification : 08.02.97           
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
      SUBROUTINE TRANSF2(A,b,c,x,x2,N,m)
C
C     Déclaration des variables
C     -------------------------
      IMPLICIT REAL*8(a-h,o-z)
      REAL*8 A(*),b(*),C(N,m),x(*),x2(*)

c     Récupération des x à partir des lamda
c     -------------------------------------
      ii=1
      DO i=1,N
         x(i)=b(i)/A(ii)
         DO l=1,m
            x(i)=x(i)-c(i,l)/A(ii)*x2(l)
         ENDDO
         ii=ii+i+1
      ENDDO
C
      RETURN
      END