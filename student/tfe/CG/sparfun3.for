C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C    Sous-routines & fonctions matricielles élémentaires
C
C                   (Version SPARSE MATRIX)
C
C    Last update : 17.11.96
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


C------------------------------------------------------------
C Calcule c = A * b    (sparse matrix - CSR format)
C------------------------------------------------------------
      SUBROUTINE SMMV(N, A, IA, JA, b, c)
         INTEGER N, i, j, IA(*), JA(*)
         REAL*8 A(*), b(*), c(*), t

         DO 1000 i=1,N
            t=0.0d0
            DO 1010 j=IA(i), IA(i+1)-1
               t=t+A(j)*b(JA(j))
1010        CONTINUE
            c(i)=t
1000     CONTINUE
         RETURN
      END
C
C------------------------------------------------------------
C Calcule c = A^T * b    (sparse matrix - CSR format)
C------------------------------------------------------------
      SUBROUTINE SMMVT(N, A, IA, JA, b, c)
         INTEGER N, i, j, IA(*), JA(*)
         REAL*8 A(*), b(*), c(*)
C
         DO i=1,N
            c(i)=0.0D0
         ENDDO
C
         DO i=1,N
            DO j=IA(i), IA(i+1)-1
               k=JA(j)
               c(k)=c(k)+A(j)*b(i)
            ENDDO
         ENDDO
         RETURN
      END
C
C------------------------------------------------------------
C Fournit H(i,j)                     (où H est triu + diag)
C------------------------------------------------------------
      REAL*8 FUNCTION HCALL(H,i,j)
         REAL*8 H(*)
         INTEGER i,j

         HCALL=H((j-1)*(j+2)/2+i)
         RETURN
      END

C------------------------------------------------------------
C Effectue H(i,j):=alpha             (où H est triu + diag)
C------------------------------------------------------------
      SUBROUTINE HASSIGN(H,i,j,alpha)
         REAL*8 H(*), alpha
         INTEGER i,j
          
         H(((j-1)*(j+2))/2+i)=alpha
         RETURN
      END