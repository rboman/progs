C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C     MATFUN.FOR : Routines de manipulations matricielles
C
C Dernière modification : 02.01.96           
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C     ---------------------------------------------------------
C        Produit d'une matrice par un vecteur (V2 = S * V1)
C     ---------------------------------------------------------
C
      SUBROUTINE TLVECT(S,V1,V2,L)
C
         IMPLICIT REAL*8 (A-H,O-Z)
         DIMENSION S(1),V1(L),V2(L)
C
         ZERO=0.D 00
         V2(1)=S(1)*V1(1)
         IF (L-1) 4,4,1
   1     IJ=1
         DO 3 I=2,L
            V2(I)=ZERO
            I1=I-1
            DO 2 J=1,I1
               IJ=IJ+1
               V2(I)=V2(I)+S(IJ)*V1(J)
   2           V2(J)=V2(J)+S(IJ)*V1(I)
            IJ=IJ+1
   3        V2(I)=V2(I)+S(IJ)*V1(I)
C
   4  RETURN
      END
C
C     ---------------------------------------------------------
C         Produit scalaire de deux vecteurs (DOTPRO = A . B) 
C     ---------------------------------------------------------
C
      FUNCTION DOTPRO (A,B,M)
C
         IMPLICIT REAL*8 (A-H,O-Z)
         DIMENSION A(1),B(1)
C
         DOTPRO=0.D00
         DO 1 I=1,M
   1        DOTPRO=DOTPRO+A(I)*B(I)
C
      RETURN
      END
