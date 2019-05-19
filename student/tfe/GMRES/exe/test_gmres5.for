      PROGRAM TESGMRE4
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C   SOLVER : GMRES for sparse matrix             12.12.96
C            (Restarted version + precond.)
C
C   Ce programme teste la routine 'gmres' avec
C   ILUT, ILUTP, ILU0, MILU0, ILUD, ILUDP
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C Compilation : necessite GMRES4.OBJ, MATFUN.OBJ, LOADMAT.OBJ
C ~~~~~~~~~~~             SPARFUN2.OBJ + precondit.
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


C     -- Declarations ---------------------------------------

      INTEGER N, m, IA, JA, NELEM, ierr, iwk, 
     #        lfil, jw, FLAG, JLU, JU, mbloc, iperm

      parameter(N = 100, NELEM = 3500, m = 100, iwk= 10000)

      REAL*8 epsilon, it_max, rbf,
     #       A, y, ALU, droptol, permtol,
     #       b, x, V, H, w, co, si, g, alph, xref

      DIMENSION A(NELEM), IA(N+1), JA(NELEM),
     #          b(N), x(N), V(N,m+1), y(N), ALU(iwk),
     #          H(m+1,m), w(N), co(m), si(m), g(m+1),
     #          JLU(iwk), JU(N), jw(2*N), iperm(2*N),xref(N)

C     -- Chargement de la matrice A et du vecteur b ---------
      CALL loadmat(A,IA,JA,b,xref,ierr)

C     -- Initialisation du vecteur x � 0.0d0 ----------------
      CALL vectinit(N,x)

C     -- Param�tres du GMRES --------------------------------
      epsilon = 1.0D-8
      it_max  = 300

C     -- Param�tres du pr�conditionneur ---------------------
      droptol = 0.05
      lfil    = 20
      permtol = 1
      mbloc   = N 
      alph    = 0.5     

C     -- Demande s'il faut pr�conditionner ------------------

      WRITE(*,*)
      FLAG  = 0
      write(*,*)'Preconditionner le syst. ?'
      write(*,*)' (1=ILUT, 2=ILUTP, 3=ILU0, 4=MILU0, 5=ILUD, 6=ILUDP) :'
      write(*,*)' (7=ROILU0, 8=ROILUT, 9=ROILUTP, 10=SSOR)'
      read(*,*)FLAG

C     -- Cr�ation du pr�conditionneur -----------------------

      ierr=0
      IF (FLAG.EQ.1) THEN
         WRITE(*,*)'lfil [10]?'
         READ(*,*) lfil
         WRITE(*,*)'droptol [0.1]?'
         READ(*,*) droptol
         CALL ilut(n,a,ja,ia,lfil,droptol,alu,jlu,ju,iwk,w,jw,ierr)
      ELSE IF (FLAG.EQ.2) THEN
         WRITE(*,*)'lfil [10]?'
         READ(*,*) lfil
         WRITE(*,*)'droptol [0.1]?'
         READ(*,*) droptol
         WRITE(*,*)'permtol [0.1]?'
         READ(*,*) mbloc
         WRITE(*,*)'mbloc [N] ?'
         READ(*,*) mbloc
         CALL ilutp(n,a,ja,ia,lfil,droptol,permtol,mbloc,alu,
     #              jlu,ju,iwk,w,jw,iperm,ierr)
      ELSE IF (FLAG.EQ.3) THEN
         CALL ilu0(n, a, ja, ia, alu, jlu, ju, jw, ierr)
      ELSE IF (FLAG.EQ.4) THEN
         CALL milu0(n, a, ja, ia, alu, jlu, ju, jw, ierr)
      ELSE IF (FLAG.EQ.5) THEN
         WRITE(*,*)'alph [0-1]?'
         READ(*,*) alph
         WRITE(*,*)'droptol [0.1]?'
         READ(*,*) droptol         
         CALL ilud(n,a,ja,ia,alph,droptol,alu,jlu,ju,iwk,w,jw,ierr)
      ELSE IF (FLAG.EQ.6) THEN
         WRITE(*,*)'alph [0-1]?'
         READ(*,*) alph
         WRITE(*,*)'droptol [0.1]?'
         READ(*,*) droptol
         WRITE(*,*)'permtol [0.1]?'
         READ(*,*) mbloc
         WRITE(*,*)'mbloc [N]?'
         READ(*,*) mbloc
         CALL iludp(n,a,ja,ia,alph,droptol,permtol,mbloc,alu,
     #              jlu,ju,iwk,w,jw,iperm,ierr)
      ELSEIF (FLAG.EQ.7) THEN
         rbf=0.0d0
         WRITE(*,*)'rbf ='
         READ(*,*)rbf
         CALL roilu0(n, a, ja, ia, alu, jlu, ju, jw, rbf,ierr)
      ELSEIF (FLAG.EQ.8) THEN
         WRITE(*,*)'lfil [10]?'
         READ(*,*) lfil
         WRITE(*,*)'droptol [0.1]?'
         READ(*,*) droptol
         rbf=0.0d0
         WRITE(*,*)'rbf ='
         READ(*,*)rbf
         CALL roilut(n,a,ja,ia,lfil,droptol,alu,jlu,ju,iwk,w,jw,
     #               rbf,ierr)
      ELSEIF (FLAG.EQ.9) THEN
         WRITE(*,*)'lfil [10]?'
         READ(*,*) lfil
         WRITE(*,*)'droptol [0.1]?'
         READ(*,*) droptol
         WRITE(*,*)'permtol [0.1]?'
         READ(*,*) mbloc
         WRITE(*,*)'mbloc [N] ?'
         READ(*,*) mbloc
         rbf=0.0d0
         WRITE(*,*)'rbf ='
         READ(*,*)rbf
         CALL roilutp(n,a,ja,ia,lfil,droptol,permtol,mbloc,alu,
     #              jlu,ju,iwk,w,jw,iperm,rbf,ierr)
      ELSEIF (FLAG.EQ.10) THEN
         WRITE(*,*)'omega ='
         READ(*,*)rbf
         CALL SSOR(N,A,IA,JA,ALU,JLU,JU,rbf,ierr)
      ENDIF


      IF(ierr.NE.0) THEN
         write(*,*)'erreur de pr�conditionnement !!!!!',ierr
         read(*,*)
      ENDIF
      WRITE(*,*)'Entrez m :'
      READ(*,*)mm
C     -- Appel de la sous-routine GMRES ---------------------
      
      CALL gmres(N, x, b, A, IA, JA, ALU, JLU, JU,epsilon, 
     #           it_max, mm, V, H, w, co, si, g, y, FLAG)
     
C     -- Permutation de la solution (si pivotage employ�) --

      IF(FLAG.EQ.2) THEN
         DO 101 i=1,N
            w(i)=x(iperm(n+i))
101      CONTINUE
         DO 102 i=1,N
            x(i)=w(i)
102      CONTINUE
      ENDIF

C     -- Sauvegarde du r�sultat vers MATLAB ----------------

      OPEN (UNIT = 1, FILE = 's_gmres.m', STATUS='unknown')
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
