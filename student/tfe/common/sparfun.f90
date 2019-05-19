!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
! Sous-routines & fonctions matricielles élémentaires
! (Version SPARSE MATRIX)
! 17.11.96
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


! Calcule c = A * b    (sparse matrix - CSR format)

SUBROUTINE SMMV(N, A, IA, JA, b, c)
    implicit none
    INTEGER :: N, i, j, IA(*), JA(*)
    REAL*8 :: A(*), b(*), c(*), t

    DO i=1,N
        t=0.0d0
        DO j=IA(i), IA(i+1)-1
            t=t+A(j)*b(JA(j))
        END DO
        c(i)=t
    END DO
    RETURN
END SUBROUTINE SMMV

! Calcule c = A^T * b    (sparse matrix - CSR format)

SUBROUTINE SMMVT(N, A, IA, JA, b, c)
    implicit none
    INTEGER :: N, i, j, k, IA(*), JA(*)
    REAL*8 :: A(*), b(*), c(*)

    DO i=1,N
    c(i)=0.0D0
    ENDDO

    DO i=1,N
        DO j=IA(i), IA(i+1)-1
            k=JA(j)
            c(k)=c(k)+A(j)*b(i)
        END DO
    END DO
    RETURN
END SUBROUTINE SMMVT

! Fournit H(i,j)   (où H est triu + diag)

REAL*8 FUNCTION HCALL(H,i,j)
    implicit none
    REAL*8 :: H(*)
    INTEGER :: i,j

    HCALL=H((j-1)*(j+2)/2+i)
    RETURN
END FUNCTION HCALL


! Effectue H(i,j):=alpha  (où H est triu + diag)

SUBROUTINE HASSIGN(H,i,j,alpha)
    implicit none
    REAL*8 :: H(*), alpha
    INTEGER :: i,j
    
    H(((j-1)*(j+2))/2+i) = alpha
    RETURN
END SUBROUTINE HASSIGN
