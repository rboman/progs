C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C        SOLVER : Gradient conjugue            20.10.96
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
      SUBROUTINE pcg(N, x, b, A, IA, JA, ALU,JU,JLU, epsilon,
     #              it_max,r, w, p, z, iflag)
C
C Input  : N           : Dim. du systeme.
C          x           : Vecteur initial x0.
C          b           : Second membre.
C          A           : Matrice du syst.
C          epsilon     : Precision relative du resultat.
C          it_max      : Nbre d'iterations max. a effectuer.
C          r, w, p, ax : Vecteurs (dim = N) auxiliaires.
C
C Output : x           : Solution du systeme.
C          it_max      : Nbre d'iterations effectuees.
C          epsilon     : Norme du residu.
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C     -- Declarations --------------------------------------
      IMPLICIT REAL*8(A-H,O-Z)
C
      DIMENSION A(*), IA(*), JA(*),
     #          ALU(*),JLU(*),JU(*),
     #          b(N), x(N), r(N), w(N), p(N), z(N)

C
C     -- Initialisations -----------------------------------
C
      CALL SMMV(N, A, IA, JA, x, z)
      CALL VectSub(N, b, z, r)
      IF(iflag.NE.0) THEN
         CALL LLTSOL(N, r, z, ALU, JLU, JU)
      ELSE
         CALL VectAssign(N,z,r)
      ENDIF

      rho   = ProdScal(N, z, r)
      no_it = 0
      tol   = epsilon * VectNorm(N, b)
C
      WRITE(*,200)
C
C     -- Boucle principale ---------------------------------

10    IF ((DSQRT(rho).GT.tol).AND.(no_it.LT.it_max)) THEN
         IF (no_it.EQ.0) THEN
            CALL VectAssign(N, p, z)
         ELSE
            beta = rho / rho_old
            DO 20 i = 1, N
               p(i) = z(i) + beta * p(i)
20          CONTINUE
         ENDIF
         CALL SMMV(N, A, IA, JA, p, w)
         alpha = rho / ProdScal(N, p, w)
         DO 30 i = 1, N
            x(i) = x(i) + alpha * p(i)
            r(i) = r(i) - alpha * w(i)
30       CONTINUE
         rho_old = rho
         IF(iflag.NE.0) THEN
            CALL LLTSOL(N, r, z, ALU, JLU, JU)
         ELSE
            CALL VectAssign(N,z,r)
         ENDIF
         rho     = ProdScal(N, z, r)
         no_it   = no_it + 1
         WRITE(*,210) no_it, DSQRT(rho)
         goto 10
      ENDIF


C-- Commentaires finaux ------------------------------------

      WRITE(*,*)
      IF (no_it.EQ.it_max) THEN
         WRITE(*,*)'Nbre max d it atteint -> divergence possible !'
      ELSE
         WRITE(*,*)'Convergence !'
      ENDIF
C
      RETURN

200   FORMAT(/' SOLVEUR Gradient Conjugue'
     #       /' -------------------------'/)
210   FORMAT(' Iteration :',T13,I4,'   Residu :',T31,D10.5)
      END

