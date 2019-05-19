      PROGRAM TCGS
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C   SOLVER : CGS (d'apres TEMPLATES)                 11.02.97
C
C   Utilise ILU0 & ILUTP
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C Compilation : necessite CGS.OBJ, MATFUN.OBJ, LOADMAT.OBJ
C ~~~~~~~~~~~             SPARFUN2.OBJ + precondit.
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C     -- Declarations ---------------------------------------
C
      IMPLICIT REAL*8(A-H,O-Z)
C
      parameter(N = 100, NELEM = 5000, iwk= 10000)

      DIMENSION A(NELEM), IA(N+1), JA(NELEM),
     #          ALU(iwk),JLU(iwk), JU(N),
     #          B(N),X(N),iw(2*N),
     #          r(N),r2(N),p(N),p2(N),q(N),q2(N),u(N),v(N),
     #          iperm(2*N),xref(N)

      ierr=0

C     -- Chargement de la matrice A et du vecteur b ---------
      CALL loadmat(A,IA,JA,b,xref,ierr)

C     -- Param�tres du SYMMLQ -------------------------------
      epsilon = 1.0D-8
      it_max  = 800
      iflag   = 2
      ierr    = 0

C     -- Param�tres du pr�conditionneur ---------------------
      droptol = 0.0
      lfil    = 40
      permtol = 0.1
      mbloc   = N 
      
      IF(iflag.EQ.1) THEN
         CALL ilu0(n, a, ja, ia, alu, jlu, ju, iw, ierr)
      ENDIF
      IF(iflag.EQ.2) THEN
         CALL ilutp(n,a,ja,ia,lfil,droptol,permtol,mbloc,alu,
     #              jlu,ju,iwk,r,iw,iperm,ierr)
      ENDIF
      WRITE(*,*)ierr
C
C     -- Appel de la sous-routine BICGSTAB -----------------
C
      CALL CGS(N, x, b, A, IA, JA, ALU, JLU, JU, 
     #         epsilon,it_max,r,r2,p,p2,q,q2,v,u,
     #         IFLAG,ierr)
C
C     -- Permutation de la solution (si pivotage employ�) --

      IF(IFLAG.EQ.2) THEN
         DO 101 i=1,N
            r(i)=x(iperm(n+i))
101      CONTINUE
         DO 102 i=1,N
            x(i)=r(i)
102      CONTINUE
      ENDIF

C     -- Sauvegarde du r�sultat vers MATLAB ----------------

      OPEN (UNIT = 1, FILE = 's_gmres.m', STATUS='UNKNOWN')
      DO 100 i = 1, N
         WRITE(1,*)'x(',i,')=',x(i),';'
100   CONTINUE
      CLOSE (UNIT = 1)

C     -- Visualisation de la solution ----------------------

      WRITE(*,*)
      WRITE(*,*)' <ENTER> pour voir la solution'
      READ(*,*)
      CALL VectAff2(N, x, xref)

      END
