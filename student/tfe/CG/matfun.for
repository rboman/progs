C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C    Sous-routines & fonctions matricielles élémentaires
C
C    Last update : 17.11.96
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


C------------------------------------------------------------
C Calcule c = A * b    (full matrix)
C------------------------------------------------------------
      SUBROUTINE MatMult(N, A, b, c)
         INTEGER N, i, j
         REAL*8 A(N,N), b(N), c(N)

         DO 1000 i=1,N
            c(i)=0.0D0
            DO 1010 j=1,N
               c(i)=c(i)+A(i,j)*b(j)
1010        CONTINUE
1000     CONTINUE
         RETURN
      END

C------------------------------------------------------------
C Calcule ProdScal = v2 * v2
C------------------------------------------------------------
      REAL*8 function ProdScal(N, v1, v2)
         INTEGER N, i
         REAL*8 v1(*), v2(*), pro

         pro=0.0D0
         DO 1020 i=1,N
            pro=pro+v1(i)*v2(i)
1020     CONTINUE
         ProdScal=pro
         RETURN
      END

C------------------------------------------------------------
C Calcule VectNorm = norm2(v1)
C------------------------------------------------------------
      REAL*8 function VectNorm(N,v1)
         INTEGER N
         REAL*8 v1(*), ProdScal

         VectNorm = DSQRT(ProdScal(N, v1, v1))
         RETURN
      END

C------------------------------------------------------------
C Calcule v3 = v1 + v2
C------------------------------------------------------------
      SUBROUTINE VectAdd(N,v1,v2,v3)
         INTEGER N, i
         REAL*8 v1(N), v2(N), v3(N)

         DO 1040 i=1,N
            v3(i)=v1(i)+v2(i)
1040     CONTINUE
         RETURN
      END

C------------------------------------------------------------
C Calcule v3 = v1 - v2
C------------------------------------------------------------
      SUBROUTINE VectSub(N,v1,v2,v3)
         INTEGER N, i
         REAL*8 v1(N), v2(N), v3(N)

         DO 1050 i=1,N
            v3(i)=v1(i)-v2(i)
1050     CONTINUE
         RETURN
      END

C------------------------------------------------------------
C Calcule v2 = alpha * v1    (alpha réel)
C------------------------------------------------------------
      SUBROUTINE VectMul(N,v1,alpha,v2)
         INTEGER N, i
         REAL*8 v1(N), v2(N), alpha

         DO 1060 i=1,N
            v2(i)=v1(i)*alpha
1060     CONTINUE
         RETURN
      END

C------------------------------------------------------------
C Copie le vecteur v2 dans v1  (v1:=v2)
C------------------------------------------------------------
      SUBROUTINE VectAssign(N,v1,v2)
         INTEGER N, i
         REAL*8 v1(*), v2(*)

         DO 1070 i=1,N
            v1(i)=v2(i)
1070     CONTINUE
         RETURN
      END

C------------------------------------------------------------
C Affiche le vecteur v1 et attend un <ENTER>
C------------------------------------------------------------
      SUBROUTINE VectAff(N,v1)
         INTEGER N, i
         REAL*8 v1(*)

         DO 1080 i = 1, N
            WRITE(*,*) i,':',v1(i)
1080     CONTINUE
         write(*,*)'<ENTER>'
         read(*,*)
         RETURN
      END

C------------------------------------------------------------
C Initialise la matrice A(N,M) à 0.0d0   (full matrix)
C------------------------------------------------------------
      SUBROUTINE MatInit(N, M, A)
         INTEGER N, M, i, j 
         REAL*8 A(N,M)

         DO 1090 i = 1, N
            DO 1100 j = 1, M
               A(i,j) = 0.0D0
1100        CONTINUE
1090     CONTINUE
         RETURN
      END

C------------------------------------------------------------
C Initialise le vecteur v à 0.0d0
C------------------------------------------------------------
      SUBROUTINE VectInit(N, v)
         INTEGER N, i 
         REAL*8 v(N)
      
         DO 1110 i = 1, N          
            v(i) = 0.0D0
1110     CONTINUE
         RETURN
      END
