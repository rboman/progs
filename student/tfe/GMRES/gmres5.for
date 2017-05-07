C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C   SOLVER : GMRES for sparse matrix             12.12.96
C            (Restarted version)
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C Updates : 27.10.96 : GMRES restarted (full matrix)
C ~~~~~~~   16.11.96 : GMRES restarted (sparse matrix)
C                        - compres. de H et A.
C                        - essai de réorthogonalisation.
C                        - optimisation de la mémoire (temp)
C                          et du temps C.P.U.
C                        - adaptation en REAL*8.
C           17.11.96 : - A au format CSR
C                      - précond. SSOR.
C           08.12.96 : - précond. ILU(0)
C           12.12.96 : - précond. ILUT, ILUTP, ILU0, MILU0,
C                                 ILUD, ILUDP (de Saad)
C                      - nettoyage (pas transf. vers Matlab)
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
      SUBROUTINE gmres(N, x, b, A, IA, JA, ALU, JLU, JU,epsilon, 
     #                 it_max, m, V, H, w, co, si, g, y, FLAG)
C
C Input  : N           : Dim. du système.
C ~~~~~    x (N)       : Vecteur initial x0.
C          b (N)       : Second membre.
C          A,IA,JA     : Matrice du syst. au format CSR.
C             .A  (NELEM) = éléments non nuls.
C             .JA (NELEM) = colonnes corresp.
C             .IA (N+1)   = pointeur vers les sauts de ligne.
C          ALU, JLU,JU : préconditionneur (MSR).
C             .ALU(PRESIZE) = éléments du précondit.
C             .JLU(2*N+1)   = pointeur vers les élém. diag.
C             .JU (N)       = pointeur vers début de U.
C          epsilon     : Précision relative du résultat.
C          it_max      : Nbre d'itérations max. à effectuer.
C          m           : paramètre 'restart'   (m<N) .
C          autres...   : Variables auxiliaires:
C             - V (N,m)   : contient les vecteurs de la base de Krylov.
C             - H ((m*(m+3))/2) : Mat. d'Hessenberg (format vect.).
C             - w, y (N)  : Vecteurs temporaires (r, w, x, Ax).
C             - co,si (m) : Coeff. de la matrice de rotation.
C             - g (m+1)   : Second membre de Hy=g.
C          FLAG        : =1 -> précond. actif.
C
C Output : x           : Solution du système Ax=b.
C ~~~~~~
C Variables : no_it       : compteur d'itération.
C ~~~~~~~~~   i,j,k,kk    : compteurs de boucle
C             temp        : var. temporaire.
C             normV,normW : normes de AVi et w.
C             sizeH       : contient (m*(m+3))/2.
C             residu      : ...
C             
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C Compilation : nécessite MATFUN.OBJ,SPARFUN2.OBJ
C ~~~~~~~~~~~             PREFUN.OBJ
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

C     -- Déclarations --------------------------------------
      implicit real*8(a-h,o-z)

      INTEGER N, i, j, k, no_it, m, IA, JA, JLU,
     #        JU, FLAG

      REAL*8 epsilon, it_max, temp, residu, tol,
     #       A, ALU,
     #       b, x, V, H, w, co, si, g, normV, normW,
     #       VectNorm, y

      DIMENSION A(*), ALU(*), IA(*), JA(*), JLU(*), JU(*),
     #          b(N), x(N), V(N,m+1), y(N),
     #          H(m+1,m), w(N), co(m), si(m), g(m+1)


C     -- Initialisations -----------------------------------
      open(UNIT = 1, FILE = 'ite.m', STATUS = 'unknown')

      tol   = epsilon*VectNorm(N, b)

C     -- Début de la boucle de 'restart'--------------------

1     no_it = 0
      j     = 0 

      CALL MatInit(m+1,m, H)
      CALL MatInit(N, m+1, V)
      CALL VectInit(m+1, g)
      CALL VectInit(N, w)

      CALL SMMV(N, A, IA, JA, x, w)
      CALL VectSub(N, b, w, w)

      temp  = VectNorm(N, w)
      g(1)  = temp
   
      DO 10 i = 1, N
         V(i,1)=w(i)/temp
10    CONTINUE

C     -- Boucle principale ---------------------------------

C                --------------------------------------
C                Base d'Arnoldi (Modified Gram Schmidt)
C                --------------------------------------
       
5      j = j+1
       WRITE(*,210)no_it, g(j)
       no_it = no_it+1
       WRITE(1,*)'residu(',no_it,')=',g(j),';'
C Préconditionnement
       
       IF (FLAG.NE.0) THEN
         CALL LUSOL(N, V(1,j), y, ALU, JLU, JU)
       ELSE
         DO 2 i=1,N
            y(i)=V(i,j)
2        CONTINUE
       ENDIF

C Calcul de Av.

         DO 30 i=1,N          
            temp=0.0D0
            DO 40 k=IA(i), IA(i+1)-1
               temp = temp + A(k) * y(JA(k))
40          CONTINUE
            w(i) = temp
30       CONTINUE

C Calcul de la norme de Av.
          
         NormV=0.0D0
         DO 31 i=1,N
            NormV=NormV+w(i)*w(i)
31       CONTINUE
         NormV=DSQRT(NormV)

C Calcul du nouveau v. (orthog. aux autres).
    
         DO 50 i=1, j
            temp=0.0D0
            DO 60 k=1, N
               temp=temp+w(k)*V(k,i)
60          CONTINUE          
            DO 70 k=1, N
               w(k)=w(k)-temp*V(k,i)
70          CONTINUE
            H(i,j)=temp
50       CONTINUE

         NormW = VectNorm(N, w)
         H(j+1,j)=NormW

C Réorthogonalisation ?

      IF (DABS(NormV-NormW).LT.1D-1) THEN
C         write(*,*)DABS(NormV-NormW)
         DO 52 i=1, j
            temp=0.0
            DO 62 k=1, N
               temp=temp+w(k)*V(k,i)
62          CONTINUE          
            DO 72 k=1, N
               w(k)=w(k)-temp*V(k,i)
72          CONTINUE
            temp=temp+H(i,j)
            H(i,j)=temp
52       CONTINUE

         NormW = VectNorm(N, w)
         H(j+1,j)=NormW
         WRITE(*,*)'-> Reorthogonalisation <-'

       ENDIF

C Teste le Happy breakdown et norme le vecteur v.
  
         IF (NormW.LT.tol) THEN
            WRITE(*,*)'-> Happy Breakdown ! <-'        
         ELSE
            DO 80 i=1, N
               V(i,j+1)=w(i)/NormW
80          CONTINUE
         ENDIF

C      ------------------------------------------------------   
C      Solver du système de Hessenberg (par rotations planes)
C      ------------------------------------------------------

C Rotation de la dernière colonne

         
         DO 90 i=1, j-1
            temp   =  H(i,j)*co(i) + H(i+1,j)*si(i)
            H(i+1,j) = -H(i,j)*si(i) + H(i+1,j)*co(i)
            H(i,j)   =  temp
90       CONTINUE

C Calcul de la nouvelle rotation (si et co).
C (Ici k pointe vers H(j,j))
         
         temp=DSQRT(H(j,j)*H(j,j)+H(j+1,j)*H(j+1,j))
         si(j)=H(j+1,j)/temp
         co(j)=H(j,j)/temp

C Rotation. 
         
         temp     =  H(i,j)*co(i) + H(i+1,j)*si(i)
         H(i+1,j) = -H(i,j)*si(i) + H(i+1,j)*co(i)
         H(i,j)   =  temp            
         temp     =  g(i)*co(i) + g(i+1)*si(i)
         g(i+1)   = -g(i)*si(i) + g(i+1)*co(i)
         g(i)     =  temp

C Test du residu

         residu = DABS(g(j+1))
         IF (residu.LT.tol) GOTO 99
         IF (no_it.EQ.it_max) GOTO 99
         IF (j.EQ.m) GOTO 99

20    GOTO 5

C     -- SORTIE ou restart ---------------------------------

C          ---------------------------------------------
C          Calcul de la solution  (substitution arrière)
C             du système H w = g avec H de dim. j*j.
C          ---------------------------------------------

99    DO 300 i = j, 1, -1
         temp=0.0
         DO 310 k = i+1, j, 1
            temp=temp+H(i,k)*w(k)
310      CONTINUE
         w(i)=(g(i)-temp)/H(i,i)
300   CONTINUE

C                   --------------------------
C                   Mise à jour de la solution
C                   --------------------------
      DO 320 i=1, N
         temp=0.0D0
         DO 330 k=1, j
            temp=temp+V(i,k)*w(k)
330      CONTINUE
         y(i)=temp
320   CONTINUE

C Préconditionnement

      IF (FLAG.NE.0) THEN
          CALL LUSOL(N, y, y, ALU, JLU, JU)
      ENDIF

      DO 340 i=1,N
         x(i)=x(i)+y(i)
340   CONTINUE

      WRITE(*,210)no_it, residu

C >>>>>>>>>>--- Calcul des vp ---<<<<<<<<<<<<<
C      CALL condi(H,j,m,co,si,conda)
      DO i=1,j
        H(i+1,i)=0.0D0
      ENDDO
      write(*,*)' calcul des vp:'
46    DO i=j,1,-1
            h3=h(i  , 1  )
            h4=h(i+1, 1  )
            c=co(i)
            s=si(i)
         DO k=1,j
            h1=h3
            h2=h4
            IF(k.NE.j) THEN
               h3=h(i  , k+1)
               h4=h(i+1, k+1)
               h(i  , k+1) = c*h3-s*h4
               h(i+1, k+1) = s*h3+c*h4
            ENDIF
            h(i  , k  ) = c*h1-s*h2
            h(i+1, k  ) = s*h1+c*h2

         ENDDO
      ENDDO

45    ierr=0
      CALL hqr(m+1,j,1,j,H,co,si,ierr)
      WRITE(*,*)'ierr=',ierr
      xlmin=1.0D100
      xlmax=0.0D0
      DO i=1,j
c         write(*,*)co(i),'+ i ',si(i)
c         read(*,*)
         co(i)=DSQRT(co(i)*co(i)+si(i)*si(i))
         if(co(i).GT.xlmax) xlmax=co(i)
         if(co(i).LT.xlmin) xlmin=co(i)
      ENDDO
      read(*,*)
      xprec=1.0D-10
c      CALL PUISS(N,A,IA,JA,ALU,JLU,JU,W,Y,xxlmax,500,
c     *           xprec,ierr,flag)
      WRITE(*,*)
      WRITE(*,*)'lmin      =',xlmin
      WRITE(*,*)'lmax      =',xlmax
      WRITE(*,*)'lmax/lmin =',xlmax/xlmin
C
      WRITE(*,*)'lmax (P)  =',xxlmax,ierr
C >>>>>>>>>>--- Calcul des vp ---<<<<<<<<<<<<<

47    IF (j.EQ.m) THEN
         WRITE(*,*)'-> Restart <ENTER>:'
         CALL SMMV(N, A, IA, JA, x, w)
         CALL VectSub(N, b, w, w)
         write(*,*)'residu vrai =', VectNorm(N,w)
         read(*,*)
         GOTO 1
      ENDIF

C     -- Commentaires finaux -------------------------------

999   WRITE(*,*)
      IF (no_it.EQ.it_max) THEN
         WRITE(*,*)'Nbre max d it atteint -> divergence possible !'
      ELSE
         WRITE(*,*)'Convergence !'
      ENDIF

C Affiche le residu vrai :

      CALL SMMV(N, A, IA, JA, x, w)
      CALL VectSub(N, b, w, w)
      write(*,*)'residu vrai =', VectNorm(N,w)

      close(unit=1)

      RETURN

210   FORMAT(' Iteration :',T14,I4,'   Residu :',T32,D22.15)

      END


