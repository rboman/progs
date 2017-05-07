C==================================================================
C Sous routine ANA : 
C    . Evaluation de la solution analytique du problème
C    . Sauvegarde dans uan.m
C
C Modifié le 11.04.97
C==================================================================
C
      SUBROUTINE ANA(hh,cond,rhoc,N,L,Tf,NT,dt,dx,
     #               ISAV,MUL,TFIN)
C
      IMPLICIT REAL*8(A-H,O-Z)
      REAL*8 L,bm(200),C(200)
C
C     Initialisation
C     -------------- 
      H   = hh/cond
      eta = cond/rhoc
      NX  = N
      pi  = 4.0D0*DATAN(1.0D0)
C
C                         .On calcule la solution uniquement
C                          pour les pas sauvegardés
      dt  = DBLE(ISAV)*dt
      NT  = NT/ISAV
C                         .Nombre de termes (2° formule)
      NC  = 100
C                         .Nombre de bm à calculer
      NZ  = 150
C                         .Tolérence de bissection
      tol = 1.0D-09
C
C     Ouverture du fichier
C     --------------------
      OPEN(UNIT=2,FILE='uan.m',STATUS='UNKNOWN')
C
C     Recherche des zeros
C     -------------------
      WRITE(*,*)'Calcul de la solution analytique'
      WRITE(*,*)'Recherche des racines'
C
      DO i=1,NZ
C         WRITE(*,*)i
         x1=(DBLE(i)-1+1.0D-10)*pi/L
         x2=(DBLE(i)-1-1.0D-10)*pi/L+pi/L/2.0D0
         f1=fct(x1,L,H)
 10         IF (DABS(f1).LT.tol) GOTO 20
            x3=(x2+x1)/2.0D0
            f3=fct(x3,L,H)
            IF((f3*f1).GT.(0.0D0))THEN
               x1=x3
               f1=fct(x1,L,H)
            ELSE
               x2=x3
            ENDIF
            GOTO 10
 20      x1=(x1+x2)/2.0D0
         bm(i)=x1
C         WRITE(2,*)'bm(',i,')=',bm(i),';'
      ENDDO
C
C     Calcul de la solution analytique (montée)
C     -----------------------------------------
C     WRITE(*,*)'Calcul des pas de temps : h=h'
      DO k=1,NT+1
         WRITE(*,*)'k =',(k-1)*ISAV
         t=(k-1)*dt
         DO j=1,NX+1
            x=(j-1)*dx
            sum=0
            DO i=1,NZ
               sum=sum+1.0D0/(L*(bm(i)**2+H**2)+H)
     #                *cos(bm(i)*x)/cos(bm(i)*L)
     #                *exp(-bm(i)**2*eta*t)
            ENDDO
            ua=Tf-2*Tf*H*sum
            WRITE(2,*)'ua(',j,',',k,')=',ua,';'
         ENDDO
         WRITE(2,*)'t2(',k,')=',t,';'
      ENDDO
C
C     Calcul de la solution analytique (descente)
C     -------------------------------------------
C     WRITE(*,*)'Calcul des pas de temps : h=0'
C
C     -- Calcul de C0 --
      C0=0
      DO i=1,NZ        
         C0=C0+1.0D0/(L*(bm(i)**2+H**2)+H)
     #        *exp(-(bm(i))**2*eta*TFIN)/cos(bm(i)*L)
     #        *sin(bm(i)*L)/bm(i)
      ENDDO
      C0=Tf+C0*(-2.0*Tf*H*1.0D0/L)
      WRITE(*,*)' .. C0 = ',C0
C
C     -- Calcul des Cn --
      DO nn=1,NC
         C(nn)=0.0D0
         DO i=1,NZ
            C(nn)=C(nn)+1.0D0/(L*(bm(i)**2+H**2)+H)
     #           *exp(-(bm(i))**2*eta*TFIN)/cos(bm(i)*L)
     #           *(-1.0D0)**(nn+1)*bm(i)*(L/DBLE(nn)/pi)**2
     #           *sin(bm(i)*L)
     #           /(1.0D0-(bm(i)*L/DBLE(nn)/pi)**2)
         ENDDO
         C(nn)=C(nn)*(-4.0D0*Tf*H/L)
      ENDDO
C
C     -- Pas de temps --
      DO k=NT+2,(MUL*NT)
         WRITE(*,*)'k =',(k-1)*ISAV
         t=(k-1)*dt
         DO j=1,NX+1
            x=(j-1)*dx
            ua=C0
            DO nn=1,NC
               ua=ua+C(nn)*cos(nn*pi*x/L)
     #              *exp(-(nn*pi/L)**2
     #              *eta*(t-TFIN))
            ENDDO
            WRITE(2,*)'ua(',j,',',k,')=',ua,';'
         ENDDO
         WRITE(2,*)'t2(',k,')=',t,';'
      ENDDO
C
C     Fermeture du fichier
C     --------------------
      CLOSE(UNIT=2)
C
      RETURN
      END 
C
C----------------------------------------------------------
      REAL*8 FUNCTION fct(x,L,H)
      REAL*8 x,L,H
C      
      fct=x*DTAN(x*L)-H
      RETURN
      END