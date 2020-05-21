C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C             GENDAT : Génère un fichier de données
C
C dernière modification : 08.02.97
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
      SUBROUTINE GENDAT(N, S, II, F, MEMORY, m)
C
C     Déclaration des variables
C     -------------------------
      IMPLICIT REAL*8 (A-H,O-Z)
C
      CHARACTER*20 Fichier
      DIMENSION S(*), II(*)
C
C     Ouverture du nouveau fichier
C     ----------------------------
      WRITE(*,112)
      READ(*,*)Fichier
C
      OPEN(UNIT = 1, ERR=200, FILE = Fichier, STATUS = 'UNKNOWN')
C
      WRITE(*,109)
      READ(*,*)idon
C
 10   WRITE(*,110)
      READ(*,*)N
      IF (N.LT.1) GOTO 10      
      WRITE(*,111)
      READ(*,*)m
C
C     Dimensionnement des vecteurs
C     ----------------------------
      IERR=0
      CALL ALLOC(N,II,MEMORY,IERR,m)
      IF(IERR.NE.0) GOTO 10
C
C     Entrée des données au clavier (f(x))
C     ------------------------------------
      IF(idon.EQ.0) THEN
         k=0
C        -- deuxième degré --
         DO i=1,N
            DO j=1,i
               k=k+1
               IF (i.EQ.j) THEN
                  WRITE(*,104)i
                  READ(*,*) S(k)
                  S(k)=2*S(k)
               ELSE
                  WRITE(*,103)j,i
                  READ(*,*) S(k)
               ENDIF
            ENDDO
         ENDDO
C        -- premier degré --
         DO i=1,N
            WRITE(*,105)i
            READ(*,*) itemp
            S(II(2)+i-1)=-itemp
         ENDDO
C        -- terme indépendant --
         WRITE(*,108)
         READ(*,*) F
      ELSE
C        -- Matrice A --
         k=0
         DO i=1,N
            DO j=1,i
               k=k+1
               WRITE(*,100)i,j
               READ(*,*) S(k)
            ENDDO
         ENDDO
C        -- Vecteur b --
         DO i=1,N
            WRITE(*,101)i
            READ(*,*) S(II(2)+i-1)
         ENDDO
C        -- terme indépendant --
         WRITE(*,*)'F( 0, 0) ='
         READ(*,*) F
      ENDIF
C
C     Entrée des données au clavier (X0)
C     ----------------------------------
      DO i=1,N
         WRITE(*,102)i
         READ(5,*) S(II(5)+i-1)
      ENDDO
C
C     Entrée des données au clavier (contraintes)
C     -------------------------------------------
      DO i=1,m
         DO j=1,n
            WRITE(*,106)i,j
            READ(*,*)S(II(3)+(i-1)*n+j-1)
         ENDDO
         WRITE(*,107)i
         READ(*,*)S(II(4)+i-1)
      ENDDO
C
C     Sauvegarde vers fichier data
C     ----------------------------
      WRITE(1,*)N
      WRITE(1,*)m
      DO i=1, N*(N+1)/2+2*N+(n+1)*m
         WRITE(1,*)S(i)
      ENDDO
      WRITE(1,*)F
      CLOSE(UNIT=1)
C
      RETURN
C
C     Erreur lors de l'ouverture du fichier
C     -------------------------------------
 200  WRITE(*,150)
      GOTO 10
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
 100  FORMAT(' A(',I2,',',I2,') = ')
 101  FORMAT(' b(',I2,') = ')
 102  FORMAT(' X(',I2,') = ')
 103  format(' Coefficient de x',I2,' x',I2,' = ')
 104  format(' Coefficient de x',I2,' **2',' = ')
 105  format(' Coefficient de x',I2,' = ')
 106  format(' C(',I2,',',I2,') = ')
 107  format(' Cmin(',I2,')   = ')
 108  FORMAT(' Terme independant = ')
 109  FORMAT(' Voulez-vous rentrer: 0) f(x)'/,
     #       '                   ou 1) A et b ? :')
 110  FORMAT(' Nbre de variables   :')
 111  FORMAT(' Nbre de contraintes :')
 112  FORMAT(' Nouveau fichier de données :')
 150  FORMAT(' Fichier inexistant !')
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
      END
