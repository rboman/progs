C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C       ALLOC : Allocation pseudo-dynamique de variables
C
C dernière modification : 02.01.97
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C Ouput : IERR=1 si 'Out of memory'
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
      SUBROUTINE ALLOC(N,II,MEMORY,IERR,m)
C
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION II(*)
C
      IDIM=MAX0(N,m)
C
         II(1) = 1
C     Hessien (A)
         II(2) = II(1) + N*(N+1)/2
C     Vecteur b
         II(3) = II(2) + N
C     Contraintes (C)
         II(4) = II(3) + N*m
C     Termes indépendants des contraintes (Cmin)
         II(5) = II(4) + m
C     Vecteur X
         II(6) = II(5) + N
C     Direction de recherche (Z)
         II(7) = II(6) + IDIM
C     Gradient (G)
         II(8) = II(7) + IDIM
C
C     Hessien2 (A2)
         II(9) = II(8) + IDIM*(IDIM+1)/2
C     Vecteur b2
         II(10) = II(9) + IDIM
C     Vecteur X2
         II(11) = II(10) + m
C     Vecteur ISA (integer*4)
         II(12) = II(11) + (IDIM+1)/2
C
C     Assez de mémoire ?
C     ------------------
      IF (II(11).GT.MEMORY) THEN
         WRITE(*,100)
         IERR=1
      ENDIF
C
      RETURN
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
 100  FORMAT(' Pas assez de mémoire !')
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
      END
