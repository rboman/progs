      PROGRAM TESTPCG
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C   SOLVER : PCG
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C     -- Déclarations ---------------------------------------
      IMPLICIT REAL*8(A-H,O-Z)

      INTEGER N, IA, JA, NELEM, ierr,it_max,iwk,iflag
      parameter(N = 100, NELEM = 3500,iwk=10000)

      DIMENSION A(NELEM), IA(N+1), JA(NELEM),
     #          ALU(iwk),JLU(iwk),JU(N+1),
     #          b(N), x(N), z(N),
     #          r(N), w(N), p(N),DR(N),JI(N),xref(N)
      LOGICAL JT(N)


C     -- Chargement de la matrice A et du vecteur b ---------
      CALL loadmat(A,IA,JA,b,xref,ierr)

C     -- Initialisation du vecteur x à 0.0d0 ----------------
      CALL vectinit(N,x)

C     -- Paramètres du GMRES --------------------------------
      epsilon = 1.0D-8
      it_max  = 1000

C     -- Création du préconditionneur -----------------------
      WRITE(*,*)' 1 : IC0'
      WRITE(*,*)' 2 : IC0  + M réduc. = RIC0'
      WRITE(*,*)' 3 : MIC0'
      WRITE(*,*)' 4 : MIC0 + M réduc. = RMIC0'
      WRITE(*,*)'precond ? (0 = non)'
      READ(*,*)iflag
      ierr= 0
C
      if (iflag.eq.1) then
         CALL IC0 (N,A,IA,JA,ALU,JU,JLU,JT,JI,DR,iwk,
     $             ierr)
      else if (iflag.eq.2) then
         CALL RIC0 (N,A,IA,JA,ALU,JU,JLU,JT,JI,DR,iwk,
     $             ierr)
      else if (iflag.eq.3) then
         WRITE(*,*)'alpha ?'
         READ(*,*)alpha
         CALL MIC0 (N,A,IA,JA,ALU,JU,JLU,JT,JI,DR,iwk,
     $             ierr,alpha)
      else if (iflag.eq.4) then
         WRITE(*,*)'alpha ?'
         READ(*,*)alpha
         CALL RMIC0 (N,A,IA,JA,ALU,JU,JLU,JT,JI,DR,iwk,
     $               ierr,alpha)
      endif
C     -- Appel de la sous-routine CG ---------------------
   
      CALL pcg(N, x, b, A, IA, JA, ALU,JU,JLU, epsilon,
     #         it_max,r, w, p, z, iflag)
     
C     -- Sauvegarde du résultat vers MATLAB ----------------

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
