C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C   SOLVER : BiCG                              11.02.97
C            (d'après Templates)
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
      SUBROUTINE BICG(N, x, b, A, IA, JA, ALU, JLU, JU, epsilon,
     #                it_max,z,z2,r,r2,p,p2,q,q2,IFLAG,ierr)
C
      IMPLICIT REAL*8(A-H,O-Z)
C 
      DIMENSION A(*), ALU(*), IA(*), JA(*), JLU(*), JU(*),
     #          b(N), x(N),
     #          z(N), z2(N), r(N), r2(N), p(N), p2(N),
     #          q(N), q2(N)
C
C------------------------------------------------------------
C
C     Compute r0 & tol
C     ----------------
      CALL SMMV(N, A, IA, JA, x, r)
      CALL VectSub(N, b, r, r)
C
      residu=VectNorm(N,r)
      tol=epsilon*residu
C
C     Choose r20 = r0
C     ---------------
      CALL VectAssign(N,r2,r)
C
      j=0
      open(UNIT = 1, FILE = 'ite.m', STATUS = 'unknown')
C
C------------------------------------------------------------
C     Boucle principale
C--------------------------------------------------------
C
 10   j=j+1     
C
      WRITE(*,210)j, residu
      WRITE(1,*)'residu2(',j,')=',residu,';'
C
C     Préconditionnement
C     ------------------
      IF(IFLAG.NE.0) THEN
         CALL LUSOL (n, r , z , alu, jlu, ju)
         CALL LUTSOL(n, r2, z2, alu, jlu, ju)
      ELSE
         CALL VectAssign(N,z ,r )
         CALL VectAssign(N,z2,r2)
      ENDIF
C
C     Calcul du rho et test du breakdown
C     ----------------------------------
      rho=ProdScal(N,z,r2)
      IF (dabs(rho).LT.tol) THEN
         WRITE(*,*)'Breakdown! rho =',rho

         ierr=1
         GOTO 999
      ENDIF
C
C     Calcul des directions de recherche bi-conjuguées
C     ------------------------------------------------
      IF (j.EQ.1) THEN
         CALL VectAssign(N,p,z)
         CALL VectAssign(N,p2,z2)
      ELSE
         beta=rho/rho2
         DO i=1,N
            p(i)  = z(i)  + beta * p(i)
            p2(i) = z2(i) + beta * p2(i)
         ENDDO
      ENDIF
C
C     Calcul de alpha
C     ---------------
      CALL SMMV (N, A, IA, JA, p , q )
      CALL SMMVT(N, A, IA, JA, p2, q2)
C
      alpha=ProdScal(N,p2,q)
      IF (dabs(alpha).LT.tol) THEN
         WRITE(*,*)'Breakdown! alpha =',alpha
         ierr=1
         GOTO 999
      ENDIF
      alpha=rho/alpha
C
C     Mise à jour de la solution et des résidus
C     -----------------------------------------
      DO i=1,N
         x(i)  = x(i)  + alpha * p(i)
         r(i)  = r(i)  - alpha * q(i)
         r2(i) = r2(i) - alpha * q2(i)
      ENDDO
C
      rho2=rho
C
C     Teste la convergence
C     --------------------
      residu=VectNorm(N,r)
      IF(residu.LT.tol) THEN
         WRITE(*,*)'Convergence!'
         ierr=0
         GOTO 999
      ENDIF
      IF(j.EQ.it_max) THEN
         WRITE(*,*)'it_max atteint!'
         ierr=1
         GOTO 999
      ENDIF
C
      GOTO 10
C------------------------------------------------------------
 999  close(unit=1)
      RETURN
C
 210  FORMAT(' Iteration :',T14,I4,'   Residu :',T32,D22.15)
C
      END        