      PROGRAM TCM2
C==================================================================
C       Modélisation des transferts de chaleur & matière
C       ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C                         (1°partie)
C
C Modifié le 02.04.97
C==================================================================
C Tableaux : . u      : champ des températures aux noeuds
C            . S      : matrice à inverser (implicite)
C            . s1, s2 : contiennent la décomposition LU de S
C            . g      : second membre du système EF
C
C Scalaires : . L,A       : caractéristiques géométriques du milieu
C             . cond,rhoc : caractéristiques thermiques du milieu
C             . h,Tf      : coeeficient de convection et
C                           température du fluide en x=0.
C             . N         : nombre d'éléments finis
C             . TMAX      : temps maximum
C             . ITMAX     : Nbre de pas de temps à effectuer
C             . ISAV      : Nbre de pas de temps entre deux sauv.
C             . dx,dt     :
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
C
C     - matériau
C
      A     = 1.0D-2
      L     = 2.0
      cond  = 0.01
      rhoc  = 1.0 
      uu    = 0.05
C
C     - conditions limites
C
      T0 = 20.0
      TL = 0.0
C
C     - intégrateur
C
      N     = 40
      TMAX  = 40
      ITMAX = 1000
      ISAV  = 100

      dx = L/DBLE(N)
      dt = TMAX/DBLE(ITMAX)
      Pe=DABS(uu)*dx*rhoc/cond
      
      WRITE(*,120)dx,TL,cond,dt,T0,rhoc,cond/rhoc
      WRITE(*,*)'Peclet de merde =',Pe
      WRITE(*,*)
      WRITE(*,*)'theta [0-1] ? :'
      READ(*,*)theta
      alpha=1.0D0/DTANH(Pe/2.0D0)-2.0D0/Pe
      WRITE(*,*)'alpha opti    =',alpha
      WRITE(*,*)'alpha [0-1] ? :'
      READ(*,*)alpha
      WRITE(*,*)'<SPACE>'
      READ(*,*)
C
C     Ouverture ficher resultat
C     -------------------------
      OPEN(UNIT=1,FILE='res.m',STATUS='UNKNOWN')
C
C     Initialisation des températures
C     -------------------------------
      DO i=1,N+1
         u(i)=0.0D0
      ENDDO
      u(1)=T0
      u(N+1)=TL
C
C     Calcul de S
C     -----------
      a1 = cond*A/dx
      a2 = rhoc*A*dx/6.0D0
      a3 = rhoc*uu*A/2.0D0 

      CALL EVALS(a1,a2,a3,theta,S,NN,s1,s2,N,dt,alpha)
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
      call EVALG(a1,a2,a3,u,T0,TL,theta,NN,g,N,dt,alpha)
C
C     Calcul de u(IT)
C     ----------------
      u(2)=g(2)
      DO i=3,N
         u(i)=g(i)-s2(i)*u(i-1)
      ENDDO
C
      u(N) = u(N)/s1(N) 
      DO i=N-1,2,-1
         u(i)=(u(i)-S(1,i)*u(i+1))/s1(i)
      ENDDO
C
C     Sauvegarde vers MATLAB
C     ----------------------
      IF(NSAV.EQ.ISAV) THEN
         NSAV=0
         NO=NO+1
         DO i=1,N+1
            WRITE(1,*)' u(',i,',',NO+1,')=',u(i),';'
            WRITE(1,*)'t(',NO+1,')=',IT*dt,';'
         ENDDO
         WRITE(*,100)IT,IT*dt,u(1)
      ENDIF
C
      IF(IT.NE.ITMAX) GOTO 10
C
C------------------------------------------------------------------
C                         Fin de boucle
C------------------------------------------------------------------
C
C     Fermeture du fichier
C     --------------------
      WRITE(1,*)'dx=',dx,';L=',L,';'
      WRITE(1,*)'cond=',cond,';uu=',uu,';'
      WRITE(1,*)'rhoc=',rhoc,';'
      WRITE(1,*)'T0=',T0,';TL=',TL,';'
      WRITE(1,*)'plot(0:dx:L,u)'
      CLOSE(UNIT=1)    
C
      WRITE(*,110)
C
C==================================================================
C
 100  FORMAT(' pas : ',I5,'    temps : ',D10.4, '    T(1) :',D10.4)
 110  FORMAT(/'  Execution teminee !     resultats dans res.m')
 120  FORMAT(/' dx :',D10.4,'   TL :',D10.4,'   k    :',D10.4,
     #       /' dt :',D10.4,'   T0 :',D10.4,'   rhoc :',D10.4,
     #       '   eta :',D10.4,/ )
C
      END