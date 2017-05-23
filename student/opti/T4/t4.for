      PROGRAM T4
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C                    TRAVAIL FLEURY 'T4.FOR'
C                    ~~~~~~~~~~~~~~~~~~~~~~~
C 02.01.97  . ajout de commentaires
C           . division en plusieurs fichiers
C           . réponse Travail #1 & 2
C 06.02.97  . début de réponse Travail #3
C 08.02.97  . correction Travail #3
C           . nettoyage et commentaires
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C 
C Var. : IMAX	: Taille du vecteur S
C ~~~~   S	: Vecteur d'allocation dynamique de mémoire
C                 (pour plus de détails, voir ALLOC.FOR)
C        II	: Vecteur contenant les pos. des tabl. dans S
C        MEMORY	: Mémoire libre
C        EPS	: Précision voulue
C        N	: Nbre de variables
C        m	: Nbre de contraintes
C        METH	: Méthode d'opti (1 = Steep. des.; 2 = CG)
C        METHD	: Méthode primale (0) ou duale (1)
C        NCHECK	: Fréquence d'évaluation de l'ASS  
C        IASS	: Type de stratégie des contraintes actives
C        ICONST : Problème contraint (1) ou non contraint (0)
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C Rem :.Il faut bien distinguer le contenu du fichier *.DAT
C ~~~   chargé en mémoire et le problème à résoudre. Par expl.,
C       on peut charger un fichier contenant des contraintes 
C       linéaires (matrices C et D) et demander au programme de
C       résoudre le problème non contraint. Il ignore alors 
C       ces matrices.
C      .Si on charge un Hessien non diagonal et qu'on demande
C       une formulation duale, le prog. ignore les éléments
C       hors diag.
C      .Il n'y a pas de formulation primale pour des 
C       contraintes linéaires
C 
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C     Déclaration des variables
C     -------------------------
      IMPLICIT REAL*8 (A-H,O-Z)
      PARAMETER (IMAX=10000)
C
      DIMENSION S(IMAX),II(12)
C
C     Définition des variables (clavier/fichier)
C     ------------------------------------------
      EPS=1.D-06
      MEMORY=IMAX
C

      OPEN(UNIT = 2, FILE='Mini.m', STATUS='UNKNOWN')
C
C     Choix de la méthode d'optimisation (SD - CG)
C     --------------------------------------------
   2  WRITE (*,360) 
      READ  (*,*) METH
      IF((METH.LT.0).OR.(METH.GT.1)) GOTO 2
C
C     Choix du type de problème (contraint - non contraint)
C     -----------------------------------------------------
      ICONST=1
C
C     Choix de la méthode (primale - duale)
C     -------------------------------------
      METHD=1
C
C     Choix de la stratégie des contraintes actives
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

 111  N=6
      m=15 
      CALL ALLOC(N,II,MEMORY,IERR,m)
C
C     Calcul de C :
C     -------------
      CALL INITC(S(II(3)),S(II(4)),N,m)
C
C     Calcul de x0:
C     -------------
      WRITE(*,*)'Entrez x0 [0.2] :'
      READ(*,*)X0
      DO i=1,N
         S(II(5)+i-1)=X0
      ENDDO
      DO i=1,m
         S(II(10)+i-1)=0.0D0
      ENDDO
      CALL CHKCONT(S(II(5)),S(II(3)),S(II(4)))

      W    = 0.0D0  
      ite  = 0
      ITER = 0   
C
C     -- Boucle : -----------------------------------------
C
 112  ite=ite+1
      XNORMOLD=0.0D0
      XNORM=DSQRT(DOTPRO(S(II(5)),S(II(5)),N))
      ERROR=DABS(XNORM-XNORMOLD)
      WRITE(*,*)'Erreur       :',ERROR
      WOLD=W
      CALL POIDS(S(II(5)),N,W)
      IF(DABS(WOLD-W).LT.1D-5) THEN
         CLOSE(UNIT=2)
         WRITE(*,*)'Nbre de quad. =',ite
         STOP
      ENDIF
      WRITE(*,*)'------------------------'
      READ(*,*)
C
C     Ecriture dans le fichier 'Mini.m'
C     ---------------------------------
      WRITE(2,*)'IT(',ite,')=',ITER,';'
      WRITE(2,*)'W(',ite,')=',W,';'
      WRITE(2,*)'Err(',ite,')=',ERROR,';'
      DO i=1,6
         WRITE(2,*)'X(',i,',',ite,')=',S(II(5)+i-1),';'
      ENDDO

C     Evaluation de A et b en fct de x:
C     ---------------------------------
      CALL EVALAB(S(II(1)),S(II(2)),S(II(5)),N)
C    
C     Appel de la routine de minimisation
C     -----------------------------------
      if(METHD.eq.1) then 
         CALL TRANSFO(S(II(1)),S(II(2)),S(II(3)),S(II(4)),S(II(5)),
     #                S(II(8)),S(II(9)),S(II(10)),N,m,F2)
         CALL MINIM2 (S(II(8)),S(II(10)),S(II(9)),S(II(7)),S(II(6)),
     #                EPS,F2,m,METH,IASS,NCHECK,ICONST,
     #                S(II(11)),ITER)
         CALL TRANSF2(S(II(1)),S(II(2)),S(II(3)),S(II(5)),S(II(10)),
     #                N,m)
      ELSE
         CALL MINIM2 (S(II(1)),S(II(5)),S(II(2)),S(II(7)),S(II(6)),
     #                EPS,F,N,METH,IASS,NCHECK,ICONST,S(II(11)),
     #                ITER)
      ENDIF
C
C     Affiche la solution
C     -------------------
C      WRITE(*,320)
C      READ(*,*)
      DO i=1,N
         WRITE(*,330)i,S(II(5)+i-1)
      ENDDO
      CALL CHKCONT(S(II(5)),S(II(3)),S(II(4)))
      GOTO 112
C
C      CLOSE(UNIT=1)
C      STOP
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
 300  FORMAT(/' Attention : ‚l‚ments diagonaux du Hessien ignor‚s!'/)
 310  FORMAT(/' Attention : contraintes lin‚aires ignor‚es!'/)
 320  FORMAT(/' <ENTER> pour voir la solution'/)
 330  FORMAT('  X(',I3,') = ',D20.10)
 340  FORMAT(' Active set strategy : 0 = ‚valuation … chaque it.:'/,
     #       '                     : 1 = attendre un pt stat.   :'/,
     #       '                     : 2 = ‚val. toutes les n it. :')
 350  FORMAT(/' Nbre de variables   :',I3/,
     #        ' Nbre de contraintes :',I3/)
 360  FORMAT( ' M‚thode  : 0 = STEEPEST DESCENT   :'/,
     #        '          : 1 = CONJUGATE GRADIENT :')
 370  FORMAT( ' ProblŠme : 0 = NON CONTRAINT      :'/,
     #        '          : 1 = CONTRAINT          :')
 380  FORMAT( ' M‚thode  : 0 = PRIMALE (Xi>=0)    :'/,
     #        '          : 1 = DUALE (c. lin.)    :')
 390  FORMAT( ' Fichier inexistant !')
 400  FORMAT( ' Entrez n :')
 410  FORMAT( ' Nom du fichier de donn‚es (0=clavier) :')
 420  FORMAT(/' Attention : Pas de contraintes lin. d‚finies'/,
     #        '   -> M‚thode primale utilis‚e avec Xi>=0'/)
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      END

