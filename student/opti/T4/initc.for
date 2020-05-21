C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C           Routine d'initialisation des contraintes
C
C dernière modification : 26.02.97
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
      SUBROUTINE INITC(C,d,N,m)
C   
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION C(N,m),d(m)
      REAL*8 l
C
C     Initialisation à ZERO
C     ---------------------
      DO i=1,N
         DO j=1,m
            C(i,j)=0.0D0
         ENDDO
      ENDDO
C
C     Données de la structure (défini aussi alleurs)
C     -----------------------
      P     = 1.0D5
      E     = 1.0D7
      l     = 360.0D0
      SIGMA = 20.0D3
      UMAX  = 2.0D0
C
      PLSE  = P*l/E
      RAC2  = DSQRT(2.0D0)
C
C     Contraintes sur les tensions
C     ----------------------------
      C(1,1) =  2.0 * P
      C(2,2) =  1.0 * P
      C(3,3) = RAC2 * P
      C(4,4) =  2.0 * P
      C(5,5) = RAC2 * P
      C(6,6) = RAC2 * P
C
      DO i=1,6
         d(i)=SIGMA
         C(i,i)=C(i,i)
      ENDDO
C
C     Contraintes sur les déplacements
C     --------------------------------
      C(1,7)=2.0       *PLSE*1.0D4
      C(2,7)=2.0*RAC2  *PLSE*1.0D4
      C(4,7)=4.0       *PLSE*1.0D4
      C(5,7)=2.0*RAC2  *PLSE*1.0D4
      C(1,8)=2.0       *PLSE*1.0D4
      C(6,8)=2.0*RAC2  *PLSE*1.0D4
      C(4,9)=2.0       *PLSE*1.0D4
      C(5,9)=2.0*RAC2  *PLSE*1.0D4
C
      DO i=7,9
         d(i)=UMAX*1.0D4
      ENDDO
C
C     Contraintes de non négativité des aires
C     ---------------------------------------
      DO i=1,6
         C(i,9+i) = -1.0D6
         d(9+i)   = -1.0D4
      ENDDO
C

C      DO i=1,N
C         DO j=1,m
C            WRITE(*,*)i,j,C(i,j),d(j)
C         ENDDO
C         READ(*,*)
C      ENDDO

      RETURN
      END