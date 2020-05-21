      SUBROUTINE SYMMLQ( N, A, IA, JA, ALU, JLU, JU,
     #                   B, R1, R2, V, W, X, Y,
     #                   PRECON, ITNLIM, RTOL, ISTOP)
C
      IMPLICIT REAL*8(A-H,O-Z)
C
      INTEGER  N, NOUT, ITNLIM, ISTOP, ITN, PRECON
      REAL*8   EPS, RTOL, ANORM, ACOND, RNORM, XNORM
      REAL*8   B(N), R1(N), R2(N), V(N), W(N), X(N), Y(N),
     #         ALU(*),JLU(*),JU(*),A(*),IA(*),JA(*)
C     ------------------------------------------------------------------
      REAL*8   ProdScal,VectNorm
      REAL*8   DABS, DMAX1, DMIN1, DSQRT
      INTEGER  MOD
      REAL*8   S,T,Z,B1,CS,SN,ONE,ALFA,BETA,DBAR,DIAG,
     1         EPSA,EPSR,EPSX,GBAR,GMAX,GMIN,
     2         OLDB,RHS1,RHS2,X1CG,X1LQ,ZBAR,ZERO,
     3         BETA1,BSTEP,DELTA,DENOM,EPSLN,GAMMA,
     4         TNORM,YNORM,
     5         CGNORM,ELQNRM,QRNORM,SNPROD,YNORM2
      INTEGER  I
C     ------------------------------------------------------------------
C
C
C     Initialisation
C
      NOUT=1
      EPS=1D-15
C
      ZERO   = 0.0
      ONE    = 1.0
      ISTOP  = 0
      ITN    = 0
      ANORM  = ZERO
      ACOND  = ZERO
      RNORM  = ZERO
      XNORM  = ZERO
      YNORM  = ZERO
C
      DO 50 I = 1, N
         X(I) = ZERO
         W(I) = ZERO
   50 CONTINUE
C
C     Calcul de V1 (1° vecteur de Lanczos)
C
      CALL VectAssign (N,Y,B)
      CALL VectAssign (N,R1,B)
      IF(PRECON.NE.0) CALL LLTSOL(N,R1,Y,ALU,JLU,JU)
      B1     = Y(1)
      BETA1  = ProdScal(N,R1,Y)
      IF (BETA1 .LT. ZERO) ISTOP = 6
      IF (BETA1 .LE. ZERO) GO TO 900
      BETA1  = DSQRT( BETA1 )
      S      = ONE/BETA1
C
      DO 100 I = 1, N
         V(I)  = S*Y(I)
  100 CONTINUE
C
C     Calcul de Y (2° vecteur de Lanczos)
C
      CALL SMMV(N,A,IA,JA,V,Y)
      ALFA   = ProdScal(N,V,Y)
      CALL DAXPY (N,(-ALFA/BETA1),R1,Y)
C
C     Effectue une reorthogonalisation
C
      Z      = ProdScal(N,V,Y)
      S      = ProdScal(N,V,V)
      CALL DAXPY (N,(-Z/S),V,Y)
      CALL VectAssign(N,R2,Y)
      IF (PRECON.NE.0) CALL LLTSOL(N,R2,Y,ALU,JLU,JU)
      OLDB   = BETA1
      BETA   = ProdScal(N,R2,Y)
      IF (BETA .LE. ZERO) ISTOP = 6
      IF (BETA .LE. ZERO) GO TO 900
      BETA   = DSQRT(BETA)
C
C     Montre si la reorthog. a ete interessante
C
      DENOM  = DSQRT(S) * VectNorm(N,R2)
      S      = Z/DENOM
      T      = ProdScal(N,V,R2)/DENOM
      IF (NOUT .GT. 0) WRITE(*, 1100) BETA1,ALFA
      IF (NOUT .GT. 0) WRITE(*, 1120) S,T
C
C     Initialisation des grandeurs de la boucle principale
C
      CGNORM = BETA1
      GBAR   = ALFA
      DBAR   = BETA
      RHS1   = BETA1
      RHS2   = ZERO
      BSTEP  = ZERO
      SNPROD = ONE
      TNORM  = ALFA**2
      YNORM2 = ZERO
      GMAX   = ZERO
      GMIN   = ONE/EPS
C
C     ------------------------------------------------------------------
C     Boucle principale
C     ------------------------------------------------------------------
C
C     Test de convergence
C
  300 ANORM  = DSQRT(TNORM)
      YNORM  = DSQRT(YNORM2)
      EPSA   = ANORM*EPS
      EPSX   = ANORM*YNORM*EPS
      EPSR   = ANORM*YNORM*RTOL
      DIAG   = GBAR
      IF (DIAG .EQ. ZERO) DIAG = EPSA
C
      ELQNRM = DSQRT(RHS1**2 + RHS2**2)
      QRNORM = SNPROD*BETA1
      CGNORM = QRNORM*BETA / DABS(DIAG)
C
C     Estimation du nbre de conditionnement de A
C
      DENOM  = DMIN1( GMIN, DABS(DIAG) )
      ACOND  = GMAX/DENOM
C
C     Criteres d'arret :
C
      IF (ITN    .GE. ITNLIM ) ISTOP = 5
      IF (ACOND  .GE. 0.1/EPS) ISTOP = 4
      IF (EPSX   .GE. BETA1  ) ISTOP = 3
      IF (CGNORM .LE. EPSX   ) ISTOP = 2
      IF (CGNORM .LE. EPSR   ) ISTOP = 1
C     ==================================================================
C
C     Impression ?
C
      IF (NOUT .LE.  0)          GO TO 600
      IF (N    .LE. 40)          GO TO 400
      IF (ITN  .LE. 10)          GO TO 400
      IF (ITN  .GE. ITNLIM - 10) GO TO 400
      IF (MOD(ITN,10)  .EQ.   0) GO TO 400
      IF (CGNORM .LE. 10.0*EPSX) GO TO 400
      IF (CGNORM .LE. 10.0*EPSR) GO TO 400
      IF (ACOND  .GE. 0.01/EPS ) GO TO 400
      IF (ISTOP  .NE. 0)         GO TO 400
      GO TO 600
C
C     Impression
C
  400 ZBAR   = RHS1/DIAG
      Z      = (SNPROD*ZBAR + BSTEP)/BETA1
      X1LQ   = X(1) + B1*BSTEP/BETA1
      X1CG   = X(1) + W(1)*ZBAR + B1*Z
C
      IF (    ITN     .EQ. 0) WRITE(*, 1200)
      WRITE(*, 1300) ITN,X1CG,CGNORM,BSTEP,ANORM,ACOND
      IF (MOD(ITN,10) .EQ. 0) WRITE(*, 1500)
C     ==================================================================
C
C
C     Deduit V de Y et calcule le nouveau Y pour la prochaine iteration
C
  600 IF (ISTOP .NE. 0) GO TO 800
      S      = ONE/BETA
C
      DO 620 I = 1, N
         V(I)  = S*Y(I)
  620 CONTINUE
C
      CALL SMMV (N,A,IA,JA,V,Y)
      CALL DAXPY (N,(-BETA/OLDB),R1,Y)
      ALFA   = ProdScal(N,V,Y)
      TNORM  = TNORM + (ALFA**2) + 2.0*(BETA**2)
      CALL DAXPY (N,(-ALFA/BETA),R2,Y)
      CALL VectAssign (N,R1,R2)
      CALL VectAssign (N,R2,Y)
      IF (PRECON.NE.0) CALL LLTSOL(N,R2,Y,ALU,JLU,JU)
      OLDB   = BETA
      BETA   = ProdScal(N,R2,Y)
      IF (BETA .LE. ZERO) ISTOP = 6
      IF (BETA .LE. ZERO) GO TO 800
      BETA   = DSQRT( BETA )
C
C     Calcul de la nouvelle rotation plane
C
      GAMMA  = DSQRT(GBAR**2 + OLDB**2)
      CS     = GBAR/GAMMA
      SN     = OLDB/GAMMA
      DELTA  = CS*DBAR + SN*ALFA
      GBAR   = SN*DBAR - CS*ALFA
      EPSLN  = SN*BETA
      DBAR   =         - CS*BETA
C
C     Mise a jour de X
C
      Z      = RHS1/GAMMA
      S      = Z*CS
      T      = Z*SN
C
      DO 700 I = 1, N
         X(I)  = (W(I)*S  +  V(I)*T)  +  X(I)
         W(I)  =  W(I)*SN -  V(I)*CS
  700 CONTINUE
C
C     Accumule le pas selon B et effectue le substitition arriere
C
      BSTEP  = SNPROD*CS*Z + BSTEP
      SNPROD = SNPROD*SN
      GMAX   = DMAX1( GMAX, GAMMA )
      GMIN   = DMIN1( GMIN, GAMMA )
      YNORM2 = Z**2 + YNORM2
      RHS1   = RHS2 - DELTA*Z
      RHS2   =      - EPSLN*Z
      ITN    = ITN  + 1
      GO TO 300
C
C     ------------------------------------------------------------------
C     Fin de boucle principale
C     ------------------------------------------------------------------
C
C     Calcul du point CG
C
  800 ZBAR   = RHS1/DIAG
      BSTEP  = SNPROD*ZBAR + BSTEP
      YNORM  = DSQRT(YNORM2 + ZBAR**2)
      CALL DAXPY (N,ZBAR,W,X)
C
C     Add the step along  B.
C
      BSTEP  = BSTEP/BETA1
      CALL VectAssign(N,Y,B)
      IF (PRECON.NE.0) CALL LLTSOL(N,B,Y,ALU,JLU,JU)
      CALL DAXPY(N,BSTEP,Y,X)
C
C     Calcul du residu final
C
      CALL SMMV(N,A,IA,JA,X,Y)
C
      DO 850 I = 1, N
         R1(I) = B(I) - Y(I)
  850 CONTINUE
C
      RNORM  = VectNorm(N,R1)
      XNORM  = VectNorm(N,X)
C
C     ==================================================================
C     Display final status.
C     ==================================================================
  900 IF (NOUT  .LE. 0) GO TO 950
      WRITE(*, 2000) ISTOP, ITN, ANORM, ACOND, RNORM, XNORM
      IF (ISTOP .EQ. 0) WRITE(*, 3000)
      IF (ISTOP .EQ. 1) WRITE(*, 3100)
      IF (ISTOP .EQ. 2) WRITE(*, 3200)
      IF (ISTOP .EQ. 3) WRITE(*, 3300)
      IF (ISTOP .EQ. 4) WRITE(*, 3400)
      IF (ISTOP .EQ. 5) WRITE(*, 3500)
      IF (ISTOP .EQ. 6) WRITE(*, 3600)
  950 RETURN
C
C     ------------------------------------------------------------------
 1000 FORMAT(// ' SYMMLQ.            Solution of symmetric  Ax = b'
     1 / ' N    =', I7, 6X, 
     2 / ' NOUT =', I7, 6X, 'ITNLIM =', I6, 6X, 'EPS   =', 1PE11.2, 5X,
     3   ' RTOL =', 1PE11.2)
 1100 FORMAT(/ ' beta1  =', 1PE12.2 / ' alpha1 =', 1PE12.2)
 1120 FORMAT(/ ' (V1,V2) before and after ', 1PE15.2
     1       / ' local reorthogonalization', 1PE15.2 )
 1200 FORMAT(// 5X, 'ITN', 7X, 'X1(CG)',
     1   10X, 'Norm(R)', 5X, 'BSTEP', 7X, 'Norm(A)', 3X, 'Cond(A)')
 1300 FORMAT(I8, 1PE19.10, 1PE11.2, 1PE14.5, 1P2E10.2)
 1500 FORMAT(1X)
 2000 FORMAT(/ ' EXIT SYMMLQ.',    14X, 'ISTOP =', I3, 18X, 'ITN =', I8
     1       / ' ANORM =', 1PE13.5, 6X, 'ACOND =', 1PE13.5, 5X,
     2         ' RNORM =', 1PE13.5, 6X, 'XNORM =', 1PE13.5)
 3000 FORMAT(/ ' The exact solution is  X = 0.')
 3100 FORMAT(/ ' Requested accuracy achieved, as determined by  RTOL.')
 3200 FORMAT(/ ' Reasonable accuracy achieved, given  EPS.')
 3300 FORMAT(/ ' X  has converged to an eigenvector.')
 3400 FORMAT(/ ' ACOND  has exceeded  0.1/EPS.')
 3500 FORMAT(/ ' The iteration limit was reached.')
 3600 FORMAT(/ ' MSOLVE  does not define a positive definite',
     1         ' preconditioner.')
C     ------------------------------------------------------------------
C
      END
