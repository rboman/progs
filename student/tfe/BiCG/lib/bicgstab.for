C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C   SOLVER : BiCG Stabilized                         11.02.97
C            (d'après Templates)
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
      SUBROUTINE BICGSTAB(N, x, b, A, IA, JA, ALU, JLU, JU, 
     #                    epsilon,it_max,r,r2,p,p2,s,s2,t,v,
     #                    IFLAG,ierr)
C
      IMPLICIT REAL*8(A-H,O-Z)
C 
      DIMENSION A(*), ALU(*), IA(*), JA(*), JLU(*), JU(*),
     #          b(N), x(N),
     #          r(N), r2(N), p(N), p2(N),
     #          s(N), s2(N), t(N), v(N)
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
      WRITE(1,*)'residu3(',j,')=',residu,';'
C
C     Calcul du rho et test du breakdown
C     ----------------------------------
      rho=ProdScal(N,r,r2)
      IF (rho.EQ.(0.0D0)) THEN
         WRITE(*,*)'Breakdown!'
         ierr=1
         GOTO 999
      ENDIF
C
C     Calcul de la direction de recherche
C     -----------------------------------
      IF (j.EQ.1) THEN
         CALL VectAssign(N,p,r)
      ELSE
         beta=(rho/rho2)*(alpha/womega)
         DO i=1,N
            p(i) = r(i) + beta * (p(i) - womega * v(i))
         ENDDO
      ENDIF
C
C     Préconditionnement
C     ------------------
      IF(IFLAG.NE.0) THEN
         CALL lusol(n, p, p2, alu, jlu, ju)
      ELSE
         CALL VectAssign(N,p2,p)
      ENDIF
C
      CALL SMMV(N, A, IA, JA, p2, v)
C
C     Calcul de alpha
C     ---------------
      alpha=ProdScal(N,r2,v)
      alpha=rho/alpha
C
C     Calcul de s
C     -----------
      DO i=1,N
         s(i)=r(i)-alpha*v(i)
      ENDDO
C
C     Teste la norme de s
C     -------------------
C     (à faire)

C
C     Préconditionnement
C     ------------------
      IF(IFLAG.NE.0) THEN
         CALL lusol(n, s, s2, alu, jlu, ju)
      ELSE
         CALL VectAssign(N,s2,s)
      ENDIF
C
      CALL SMMV(N, A, IA, JA, s2, t)
C
      womega=ProdScal(N,t,t)
      womega=ProdScal(N,t,s)/womega
C
C     Mise à jour de la solution et des résidus
C     -----------------------------------------
      DO i=1,N
         x(i)  = x(i)  + alpha  * p2(i) + womega * s2(i)
         r(i)  = s(i)  - womega * t(i)
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