      PROGRAM GENERATE
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C         GENERATE.FOR : Genere un problème par formule
C
C dernière modification : 02.01.97
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C Link : GENERATE.FOR, ALLOC.FOR
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

C     Déclaration des variables
C     -------------------------
      IMPLICIT REAL*8 (A-H,O-Z)
      PARAMETER(N=10)

      DIMENSION S(10000), II(6)

      MEMORY=10000
      OPEN(UNIT = 1, FILE = 'T2.dat', STATUS = 'UNKNOWN')

C     Dimensionnement des vecteurs
C     ----------------------------
      IERR=0
      CALL ALLOC(N,II,MEMORY,IERR)

C     Calcul des données
C     ------------------
      k=0
      F=0.0D0
      DO i=1,N
         DO j=1,i
            k=k+1
            S(k)=0.0D0
            IF(i.EQ.j) THEN
               S(k)=2.0/i
            ENDIF
         ENDDO
      ENDDO
      DO i=1,N
         S(II(2)+i-1)=2.0/i*(i-5.0)
         S(II(3)+i-1)=5.0
         F=F+(5.0-i)*(5.0-i)/i
      ENDDO

C     Sauvegarde vers fichier data
C     ----------------------------
      WRITE(1,*)N
      DO i=1, N*(N+1)/2+2*N
         WRITE(1,*)S(i)
      ENDDO
      WRITE(1,*)F
      CLOSE(UNIT=1)

      END