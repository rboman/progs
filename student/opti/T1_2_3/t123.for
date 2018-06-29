      PROGRAM T123
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C                    TRAVAIL FLEURY 'T123.FOR'
C                    ~~~~~~~~~~~~~~~~~~~~~~~~~
C 02.01.97  . ajout de commentaires
C           . division en plusieurs fichiers
C           . reponse Travail #1 & 2
C 06.02.97  . debut de reponse Travail #3
C 08.02.97  . correction Travail #3
C           . nettoyage et commentaires
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C 
C Var. : IMAX	: Taille du vecteur S
C ~~~~   S	: Vecteur d'allocation dynamique de memoire
C                 (pour plus de details, voir ALLOC.FOR)
C        II	: Vecteur contenant les pos. des tabl. dans S
C        MEMORY	: Memoire libre
C        EPS	: Precision voulue
C        N	: Nbre de variables
C        m	: Nbre de contraintes
C        METH	: Methode d'opti (1 = Steep. des.; 2 = CG)
C        METHD	: Methode primale (0) ou duale (1)
C        NCHECK	: Frequence d'evaluation de l'ASS  
C        IASS	: Type de strategie des contraintes actives
C        ICONST : Probleme contraint (1) ou non contraint (0)
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C Rem :.Il faut bien distinguer le contenu du fichier *.DAT
C ~~~   charge en memoire et le probleme a resoudre. Par expl.,
C       on peut charger un fichier contenant des contraintes 
C       lineaires (matrices C et D) et demander au programme de
C       resoudre le probleme non contraint. Il ignore alors 
C       ces matrices.
C      .Si on charge un Hessien non diagonal et qu'on demande
C       une formulation duale, le prog. ignore les elements
C       hors diag.
C      .Il n'y a pas de formulation primale pour des 
C       contraintes lineaires
C 
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C     Declaration des variables
C     -------------------------
      IMPLICIT REAL*8 (A-H,O-Z)
      PARAMETER (IMAX=10000)
      CHARACTER*20 Fichier
C
      DIMENSION S(IMAX),II(12)
C
C     Definition des variables (clavier/fichier)
C     ------------------------------------------
      EPS=1.D-06
      MEMORY=IMAX
C
 10   WRITE(*,410)
      READ(*,*)Fichier
      IF(Fichier.NE.'0') THEN         
         OPEN(UNIT = 1, ERR=200, FILE = Fichier, STATUS = 'OLD')
         READ(1,*)N,m
 11      WRITE(*,350)N,m
         IF(N.LT.1) GOTO 11 
         IERR=0
         CALL ALLOC(N,II,MEMORY,IERR,m)
         IF(IERR.NE.0) STOP
         DO i=1, N*(N+1)/2+2*N+(N+1)*m
            READ(1,*)S(i)
         ENDDO
         READ(1,*)F
         CLOSE(UNIT=1)
      ELSE
         CALL GENDAT(N,S,II,F,MEMORY,m)
      ENDIF
C
C     Choix de la methode d'optimisation (SD - CG)
C     --------------------------------------------
   2  WRITE (*,360) 
      READ  (*,*) METH
      IF((METH.LT.0).OR.(METH.GT.1)) GOTO 2
C
C     Choix du type de probleme (contraint - non contraint)
C     -----------------------------------------------------
   3  WRITE (*,370) 
      READ  (*,*) ICONST
      IF((ICONST.LT.0).OR.(ICONST.GT.1)) GOTO 3
C
C     Choix de la methode (primale - duale)
C     -------------------------------------
      IF(ICONST.EQ.1) THEN
   4     WRITE (*,380)
         READ  (*,*) METHD
         IF((METHD.LT.0).OR.(METHD.GT.1)) GOTO 4
         IF((METHD.EQ.1).AND.(m.EQ.0)) THEN
            WRITE(*,420)
            METHD=0
         ENDIF
         IF((METHD.EQ.0).AND.(m.NE.0)) WRITE(*,310)
         IF(METHD.EQ.1) THEN
            CALL CHKDUAL(N,A,IERR)
            IF(IERR.EQ.1) WRITE(*,300)
         ENDIF
      ELSE
         METHD=0
      ENDIF
C
C     Choix de la strategie des contraintes actives
C     ---------------------------------------------
      IF(ICONST.EQ.1) THEN
  5      WRITE(*,340) 
         READ(*,*)IASS
         IF((IASS.LT.0).OR.(IASS.GT.2)) GOTO 5
         IF(IASS.EQ.2) THEN
            WRITE(*,400)
            READ(*,*)NCHECK
         ENDIF
      ENDIF
C    
C     Appel de la routine de minimisation
C     -----------------------------------
      if(METHD.eq.1) then 
         CALL TRANSFO(S(II(1)),S(II(2)),S(II(3)),S(II(4)),S(II(5)),
     #                S(II(8)),S(II(9)),S(II(10)),N,m,F2)
         CALL MINIM2 (S(II(8)),S(II(10)),S(II(9)),S(II(7)),S(II(6)),
     #                EPS,F2,m,METH,IASS,NCHECK,ICONST,S(II(11)))
         CALL TRANSF2(S(II(1)),S(II(2)),S(II(3)),S(II(5)),S(II(10)),
     #                N,m)
      ELSE
         CALL MINIM2 (S(II(1)),S(II(5)),S(II(2)),S(II(7)),S(II(6)),
     #                EPS,F,N,METH,IASS,NCHECK,ICONST,S(II(11)))
      ENDIF
C
C     Affiche la solution
C     -------------------
      WRITE(*,320)
      READ(*,*)
      DO i=1,N
         WRITE(*,330)i,S(II(5)+i-1)
      ENDDO
C
      CLOSE(UNIT=1)
      STOP
C
C     Erreur lors de l'ouverture du fichier
C     -------------------------------------
 200  WRITE(*,390)
      GOTO 10
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
 300  FORMAT(/' Attention : elements diagonaux du Hessien ignores!'/)
 310  FORMAT(/' Attention : contraintes lineaires ignorees!'/)
 320  FORMAT(/' <ENTER> pour voir la solution'/)
 330  FORMAT('  X(',I3,') = ',D20.10)
 340  FORMAT(' Active set strategy : 0 = evaluation a chaque it.:'/,
     #       '                     : 1 = attendre un pt stat.   :'/,
     #       '                     : 2 = eval. toutes les n it. :')
 350  FORMAT(/' Nbre de variables   :',I3/,
     #        ' Nbre de contraintes :',I3/)
 360  FORMAT( ' Methode  : 0 = STEEPEST DESCENT   :'/,
     #        '          : 1 = CONJUGATE GRADIENT :')
 370  FORMAT( ' Probleme : 0 = NON CONTRAINT      :'/,
     #        '          : 1 = CONTRAINT          :')
 380  FORMAT( ' Methode  : 0 = PRIMALE (Xi>=0)    :'/,
     #        '          : 1 = DUALE (c. lin.)    :')
 390  FORMAT( ' Fichier inexistant !')
 400  FORMAT( ' Entrez n :')
 410  FORMAT( ' Nom du fichier de donnees (0=clavier) :')
 420  FORMAT(/' Attention : Pas de contraintes lin. definies'/,
     #        '   -> Methode primale utilisee avec Xi>=0'/)
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      END

