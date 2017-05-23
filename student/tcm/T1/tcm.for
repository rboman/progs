      PROGRAM TCM
C==================================================================
C       Modélisation des transferts de chaleur & matière
C       ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C                         (1°partie)
C
C Modifié le 06.04.97
C==================================================================
C Tableaux : . u      : champ des températures aux noeuds
C            . S      : matrice à inverser (implicite)
C            . s1, s2 : contiennent la décomposition LU de S
C            . g      : second membre du système EF
C
C Scalaires : . L,A       : caractéristiques géométriques du milieu
C             . cond,rhoc : caractéristiques thermiques du milieu
C             . h,Tf      : coefficient de convection et
C                           température du fluide en x=0.
C             . N         : nombre d'éléments finis
C             . ITMAX     : Nbre de pas de temps à effectuer
C                           avec h=h (échauffement)
C             . MUL       : Nbre de pas de temps total = ITMAX*MUL
C             . dx,dt     : Pas spatial et temporel
C             . IDIAG     : =1 -> diagonalisation de C
C             . ISAV      : Nbre de pas de temps entre deux sauv.
C             . NSAV      : Nbre de pas depuis la derniere sauv.
C             . NO        : Numero de la derniere sauvegarde
C==================================================================
C
      IMPLICIT REAL*8(A-H,O-Z)
      REAL*8 L
C
      PARAMETER(NN=1000)
C
      DIMENSION u(NN),S(3,NN),g(NN),s1(NN),s2(NN)
C
C------------------------------------------------------------------
C
C     Initialisation
C     --------------
      WRITE(*,*)'theta [0-1] ? :'
      READ(*,*)theta
      IF(theta.EQ.(0.0D0))THEN
         WRITE(*,*)'Diagonalisation de C (0=non) ? :'
         READ(*,*)IDIAG
      ELSE
         IDIAG=0
      ENDIF
C
C     - matériau
C
      A     = 1.0D0
      L     = 0.1D0
      cond  = 400.0D0
      rhoc  = 8900.0D0 * 384.0D0
C Cu  8900.0D0 * 384.0D0  k = 400
C Fer 7900.0D0 * 447.0D0  k = 70
C Inox 8000.0D0 * 400.0D0 k = 20
C1.0D0/(1.0D0)
C
C     - convection
C
      h     = 1.0D3
      Tf    = 100.0D0
C
C     - intégrateur
C
      N     = 30
      TFIN  = 10.0D0
      MUL   = 4
      ITMAX = 5
      ISAV  = 1
C
C     Calcul du pas spatial et temporel
C     ---------------------------------
      dx = L/DBLE(N)
      dt = TFIN/DBLE(ITMAX)
      eta=cond/rhoc
C
      WRITE(*,*)
      IF(theta.LT.0.5D0)THEN
         dtms=dx*dx/6.0D0/eta/(1-2.*theta)
         WRITE(*,*)'dt max stab  :',dtms
         IF(dt.GT.dtms) THEN
            WRITE(*,*)'   risque d''instabilite !'
            WRITE(*,*)'   ITMAX min =',TFIN/dtms 
            WRITE(*,*)'   N max     =',
     #                L/DSQRT(eta*6.0D0*dt*(1-2.*theta))  
         ENDIF 
      ELSE
         WRITE(*,*)'dt max stab  : infini'
      ENDIF
      IF(theta.LT.1.0D0)THEN
         dtmo=dx*dx/12.0D0/eta/(1-theta)
         WRITE(*,*)'dt max oscil :',dtmo
         IF(dt.GT.dtmo) THEN
            WRITE(*,*)'   risque d''oscillations !'
            WRITE(*,*)'   ITMAX min =',TFIN/dtmo 
            WRITE(*,*)'   N max     =',
     #                L/DSQRT(eta*12.0D0*dt*(1-theta))  
         ENDIF
      ELSE
         WRITE(*,*)'dt max oscil : infini'
      ENDIF
C
C     Affichage des données à l'écran
C     -------------------------------
      WRITE(*,120)dx,h,cond,dt,Tf,rhoc,cond/rhoc
      WRITE(*,*)'<SPACE>'
      READ(*,*)
C
C     Ouverture ficher resultat
C     -------------------------
      OPEN(UNIT=1,FILE='res.m',STATUS='UNKNOWN')
      WRITE(1,*)'dx=',dx,';L=',L,';N=',N,';'
      WRITE(1,*)'cond=',cond,';rhoc=',rhoc,';'
      WRITE(1,*)'h=',h,';Tf=',Tf,';'
      WRITE(1,*)'dt=',dt,';A=',A,';'
      WRITE(1,*)'theta=',theta,';'
C
C     Calcul de la solution analytique
C     --------------------------------
      dt2=dt
      ITMAX2=ITMAX
      CALL ANA(h,cond,rhoc,N,L,Tf,ITMAX2,dt2,dx,ISAV,MUL,TFIN)
      WRITE(*,130)
C
C     Initialisation des températures
C     -------------------------------
      DO i=1,N+1
         u(i)=0.0D0
      ENDDO
      IEVAL=0
C
C     Calcul de S
C     -----------
      a1 = cond*A/dx
      a2 = rhoc*A*dx/6.0D0
      CALL EVALS(a1,a2,A,theta,S,NN,s1,s2,h,N,dt,IDIAG)
C
C------------------------------------------------------------------
C                    Boucle des pas de temps
C------------------------------------------------------------------
C
      IT   = 0
      NSAV = 0
      NO=0
      DO i=1,N+1
         WRITE(1,*)'u(',i,',',NO+1,')=',u(i),';'
         WRITE(1,*)'t(',NO+1,')=',IT*dt,';'
      ENDDO
C
 10   CONTINUE
      IT=IT+1
      NSAV=NSAV+1
C
C     Calcul de g (second membre)
C     ---------------------------
      CALL EVALG(a1,a2,u,A,Tf,theta,NN,g,h,N,dt,IT,ITMAX,IDIAG,S)
C
      IF((IT.EQ.ITMAX).AND.(IEVAL.EQ.0)) THEN
         IEVAL=1
         h=0.0D0
         CALL EVALS(a1,a2,A,theta,S,NN,s1,s2,h,N,dt,IDIAG)
      ENDIF
C
C     Calcul de u(IT)
C     ---------------
      IF(IDIAG.EQ.0)THEN
         u(1)=g(1)
         DO i=2,N+1
            u(i)=g(i)-s2(i)*u(i-1)
         ENDDO
C
         u(N+1) = u(N+1)/s1(N+1) 
         DO i=N,1,-1
            u(i)=(u(i)-S(1,i)*u(i+1))/s1(i)
         ENDDO
      ELSE
         DO i=1,N+1
            u(i)=g(i)/S(2,i)
         ENDDO         
      ENDIF
C
C     Sauvegarde vers MATLAB
C     ----------------------
      IF(NSAV.EQ.ISAV) THEN
         NSAV=0
         NO=NO+1
         DO i=1,N+1
            WRITE(1,*)'u(',i,',',NO+1,')=',u(i),';'
            WRITE(1,*)'t(',NO+1,')=',IT*dt,';'
         ENDDO
         WRITE(*,100)IT,IT*dt,u(1)
      ENDIF
C
      IF(IT.NE.ITMAX*MUL-1) GOTO 10
C
C------------------------------------------------------------------
C                         Fin de boucle
C------------------------------------------------------------------
C
C     Fermeture du fichier
C     --------------------
      CLOSE(UNIT=1)
C
      WRITE(*,110)
C
C==================================================================
C
 100  FORMAT(' pas : ',I5,'    temps : ',D10.4, '    T(1) :',D10.4)
 110  FORMAT(/'  Execution teminee !     resultats dans res.m')
 120  FORMAT(/' dx :',D10.4,'   h  :',D10.4,'   k    :',D10.4,
     #       /' dt :',D10.4,'   Tf :',D10.4,'   rhoc :',D10.4,
     #       '   eta :',D10.4,/ )
 130  FORMAT(/' Solution analytique calculee ! --> uan.m'/)
C
      END