C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C             MINIM2.FOR : Routine de minimisation
C
C dernière modification : 09.02.97
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
      SUBROUTINE MINIM2(A,X,B,G,Z,EPS,F0,N,METH,IASS,NCHECK,
     #                  ICONST,ISA,ITER)

C     Déclaration des variables
C     -------------------------
      IMPLICIT REAL*8 (A-H,O-Z)
      DIMENSION A((N*(N+1))/2),X(1),B(1),G(1),Z(1),ISA(*)
C
C     Ouverture du fichier résultat
C     -----------------------------
C     OPEN(UNIT = 1, FILE = 'RES.M', STATUS = 'UNKNOWN')
C      WRITE(*,*)A
C      READ(*,*)
C
C     Définition des variables
C     ------------------------
      ZERO = 0.D 00
      TOL  = EPS*EPS
      PREC = TOL*1.D-06
C
      ZMOD = 1.0D0
      TAU  = ZERO
C
      ITER  = 0
      ITER2 = 0
      ICONJ = 0
C
      MAX=N*100
      IF(METH.EQ.1) MAX=N*2
C
C     Affichage du titre
C     ------------------
C      WRITE (6,200)
C
C     Initialisation de la direction de recherche
C     -------------------------------------------
      DO I=1,N
         Z(I)=ZERO
         ISA(I)=0
      ENDDO
C
C     ---------------------------------------------------------
C                      Boucle des itérations
C     ---------------------------------------------------------
C
   2  CONTINUE
C
C        Calcul du gradient et mise à jour de f(x)
C        Projection du gradient (si nécessaire)
C        -----------------------------------------
         CALL TLVECT (A,X,G,N)
         FCT=F0+DOTPRO(X,G,N)/2.-DOTPRO(X,B,N)
         DO I=1,N
            G(I)=G(I)-B(I)
         ENDDO
C
         IF(ICONST.EQ.1) THEN
            DO I=1,N
               IF(X(I).LT.EPS) THEN
                  IF (iass.EQ.1) G(I)=ZERO
                  IF ((iass.EQ.0).AND.(G(i).GT.0)) G(I)=ZERO
                  IF (iass.EQ.2) THEN
                     IF(ITER2.NE.NCHECK) THEN
                         G(i)=ZERO
                     ELSE
                        IF (G(i).GT.0) THEN
                           G(i)=ZERO
                        ENDIF   
                        ITER2=0 
                     ENDIF
                  ENDIF
                  IF(ISA(I).EQ.0) ICONJ=0
                  ISA(I)=1
C                  WRITE(*,202)I
               ELSE
                  ISA(I)=0
               ENDIF
            ENDDO
         ENDIF
C
C        Calcul de la norme du gradient  (=CONV)
C        Sauvegarde de l'ancienne valeur (=BR)
C        (utile uniquement pour CG)
C        ---------------------------------------
         BR=ZMOD
         ZMOD=DOTPRO(G,G,N)
         CONV=DSQRT(ZMOD)
C 
C         WRITE (6,201) ITER,FCT,CONV,X(1),X(2),Z(1),Z(2),TAU
C
C        Sauvegarde des valeurs
C        ----------------------
C         WRITE(1,*)'FCT(',ITER+1,')=',FCT,';'
C         WRITE(1,*)'G(',ITER+1,')=',CONV,';'
C         WRITE(1,*)'TAU(',ITER+1,')=',TAU,';'
C         DO i=1,N
C            WRITE(1,*)'X(',i,',',ITER+1,')=',X(i),';'
C            WRITE(1,*)'Z(',i,',',ITER+1,')=',Z(i),';'
C         ENDDO
C
         ITER=ITER+1
         ITER2=ITER2+1
C
C        Teste la convergence
C        --------------------
         IF(ITER.GT.MAX) GOTO 99
C
         IF(ICONST.EQ.0) THEN
            IF(ZMOD.LT.TOL) GOTO 99
         ELSE
            IF(ZMOD.LT.TOL) THEN
               CALL TLVECT (A,X,G,N)
               DO i=1,N
                  G(i)=G(i)-B(i)
                  IF (G(i).GT.ZERO) G(i)=ZERO
               ENDDO         
               ZMOD=DOTPRO(G,G,N)
               IF (ZMOD.LT.TOL) GOTO 99
               ICONJ=0
            ENDIF
         ENDIF
C
C        Calcul de GAM (-> dir. de recherche)
C        ------------------------------------
         GAM=ZMOD/BR
         IF(ICONJ.EQ.0) GAM=ZERO
         IF(METH.EQ.0) GAM=ZERO
         ICONJ=1
C
C        Mise à jour de la direction de recherche
C        ----------------------------------------
         DO 6 I=1,N
   6        Z(I)=-G(I)+GAM*Z(I)
C
C        Proj. orthog. de la dir. de recherche sur les 
C        contraintes actives
C        Calcul de TAU_MAX (pas max. admissible)
C        ---------------------------------------------
         IF(ICONST.EQ.1) THEN
            NB_INT=0
            DO i=1,N
               IF((Z(i).LT.ZERO)) THEN
                  IF(X(i).LT.EPS) THEN              
                     Z(i)=ZERO
                  ELSE
                     TAU=-X(i)/Z(i)
                     IF((TAU.LT.TAU_MAX).OR.(NB_INT.EQ.0)) 
     #                  TAU_MAX=TAU
                     NB_INT=NB_INT+1
                  ENDIF
               ENDIF
            ENDDO
            IF(DOTPRO(Z,Z,N).LT.TOL) GOTO 99
         ENDIF
C
C        Calcul de TAU non contraint
C        ---------------------------
         CONV=DOTPRO(Z,G,N)
         CALL TLVECT (A,Z,G,N)
         TEST=DOTPRO (Z,G,N)
         IF(TEST.LT.PREC) TEST=PREC
         TAU=-CONV/TEST
C       
C        Choix du pas (le minimum)
C        -------------------------
         IF((NB_INT.NE.0).AND.(ICONST.EQ.1)) 
     #      TAU=DMIN1(TAU,TAU_MAX)
C         READ(*,*)
C
C        Calcul du nouveau point
C        -----------------------
         DO 10 I=1,N
            X(I)=X(I)+TAU*Z(I)
  10        IF(X(I).LT.EPS) X(I)=0.0D0
      GOTO 2
C
C  99  CLOSE(UNIT=1)
C
  99   WRITE(*,*)'No it:',ITER
       RETURN
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
 200  FORMAT(/' IT   FUNCTION        |G|       X(1)       X(2)',
     *       '       Z(1)       Z(2)       TAU'/1X,79(1H-))
 201  FORMAT(I3,7F11.3)
 202  FORMAT(' contrainte #', I3,' active')
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
      END

