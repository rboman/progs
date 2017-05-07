      IMPLICIT REAL*8 (A-H,O-Z)
      DIMENSION H(3),X(2),R(2),G(2),Z(2)
      EPS=1.D-06
      N=2
      A=100.
      B=1.
      C=-2.
      D=-198.
      E=0.
      F=99.
      X(1)=1.                                                           
      X(2)=10.                                                          
      WRITE (6,*) 'ENTER FUNCTION COEFFICIENTS (0. FOR TEST CASE)'
      WRITE (6,*) 'X**2'
      READ (5,*) AA
      IF(AA.EQ.0.) GOTO 2
      A=AA
      WRITE (6,*) 'Y**2'
      READ (5,*) B
      WRITE (6,*) 'XY'
      READ (5,*) C
      WRITE (6,*) 'X'
      READ (5,*) D
      WRITE (6,*) 'Y'
      READ (5,*) E
      WRITE (6,*) '0'
      READ (5,*) F
      WRITE (6,*) 'ENTER STARTING POINT'
      READ (5,*) X(1),X(2)
   2  WRITE (6,*) 'ENTER METHOD     0 = STEEPEST DESCENT'
      WRITE (6,*) '                 1 = CONJUGATE GRADIENT'
      READ (5,*) METH
      H(1)=A*2.
      H(2)=C
      H(3)=B*2.
      R(1)=-D
      R(2)=-E
      CALL MINIM (H,X,R,G,Z,EPS,F,N,METH)
      WRITE (6,*) METH
      STOP
      END
      SUBROUTINE MINIM (A,X,B,G,Z,EPS,F0,N,METH)
      IMPLICIT REAL*8 (A-H,O-Z)
      DIMENSION A(1),X(1),B(1),G(1),Z(1)
      ZERO=0.D 00
      TOL=EPS*EPS
      PREC=TOL*1.D-06
      ZMOD=1.D 00
      TAU=ZERO
      ITER=0
      MAX=N*100
      IF(METH.EQ.1) MAX=N*2
      ICONJ=0
      WRITE (6,200)
      DO 1 I=1,N
   1  Z(I)=ZERO
C
   2  CONTINUE
      CALL TLVECT (A,X,G,N)
      FCT=F0+DOTPRO(X,G,N)/2.-DOTPRO(X,B,N)
      DO 4 I=1,N
   4  G(I)=G(I)-B(I)
C
      BR=ZMOD
      ZMOD=DOTPRO(G,G,N)
      CONV=DSQRT(ZMOD)
C
      WRITE (6,201) ITER,FCT,CONV,X(1),X(2),Z(1),Z(2),TAU
      ITER=ITER+1
      IF(ITER.GT.MAX) GOTO 99
      IF(ZMOD.LT.TOL) GOTO 99
C
      GAM=ZMOD/BR
      IF(ICONJ.EQ.0) GAM=ZERO
      IF(METH.EQ.0) GAM=ZERO
      ICONJ=1
      DO 6 I=1,N
   6  Z(I)=-G(I)+GAM*Z(I)
      CONV=DOTPRO(Z,G,N)
      CALL TLVECT (A,Z,G,N)
      TEST=DOTPRO (Z,G,N)
      IF(TEST.LT.PREC) TEST=PREC
      TAU=-CONV/TEST
C
      DO 10 I=1,N
  10  X(I)=X(I)+TAU*Z(I)
      GOTO 2
C
  99  RETURN
 200  FORMAT(' IT   FUNCTION        |G|       X(1)       X(2)',
     *       '       Z(1)       Z(2)       TAU'/1X,79(1H-))
 201  FORMAT(I3,7F11.3)
      END
      SUBROUTINE TLVECT(S,V1,V2,L)
      IMPLICIT REAL*8 (A-H,O-Z)
      DIMENSION S(1),V1(L),V2(L)
      ZERO=0.D 00
      V2(1)=S(1)*V1(1)
      IF (L-1) 4,4,1
   1  IJ=1
      DO 3 I=2,L
      V2(I)=ZERO
      I1=I-1
      DO 2 J=1,I1
      IJ=IJ+1
      V2(I)=V2(I)+S(IJ)*V1(J)
   2  V2(J)=V2(J)+S(IJ)*V1(I)
      IJ=IJ+1
   3  V2(I)=V2(I)+S(IJ)*V1(I)
   4  RETURN
      END
      FUNCTION DOTPRO (A,B,M)
      IMPLICIT REAL*8 (A-H,O-Z)
      DIMENSION A(1),B(1)
      DOTPRO=0.D 00
C
      DO 1 I=1,M
   1  DOTPRO=DOTPRO+A(I)*B(I)
C
      RETURN
      END

