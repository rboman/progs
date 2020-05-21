      PROGRAM TSYMMLQ2
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C   SOLVER : SYMMLQ
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C Compilation : nécessite SYMMLQ.OBJ, MATFUN.OBJ, LOADMAT.OBJ
C ~~~~~~~~~~~             SPARFUN2.OBJ + précondit.
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


C     -- Déclarations ---------------------------------------
C
      IMPLICIT REAL*8(A-H,O-Z)
C
      parameter(N = 100, NELEM = 5000, iwk= 5000)

      DIMENSION A(NELEM), IA(N+1), JA(NELEM),
     #          ALU(iwk),JLU(iwk), JU(N),
     #          B(N),X(N),R1(N),R2(N),V(N),W(N),Y(N),
     #          DR(N),JI(N),xref(N)
      LOGICAL JT(N), JT2(N)
      ierr=0

C     -- Chargement de la matrice A et du vecteur b ---------
      CALL loadmat(A,IA,JA,b,xref,ierr)

C     -- Paramètres du SYMMLQ -------------------------------
      epsilon = 1.0D-8
      it_max  = 500
      iflag   = 1
      ierr    = 0
      WRITE(*,*)'precond ? (0 = non)'
      READ(*,*)iflag
      ierr= 0

      IF(iflag.NE.0) THEN
         ierr=0
         CALL IC0 (N,A,IA,JA,ALU,JU,JLU,JT,jt2,JI,DR,iwk,
     $             ierr)
         WRITE(*,*)'Precond calcule !'
         READ(*,*)
      ENDIF
C     -- Appel de la sous-routine SYMMLQ -------------------
C
      CALL SYMMLQ( N, A, IA, JA, ALU, JLU, JU,
     #                   B, R1, R2, V, W, X, Y,
     #                   IFLAG, it_max, epsilon, ierr)

C     -- Sauvegarde du résultat vers MATLAB ----------------

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
