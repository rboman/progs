!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
! Sous-routines & fonctions matricielles élémentaires
! (Version SPARSE MATRIX)
! 17.11.96
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


! Calcule c = A * b    (sparse matrix - CSR format)

SUBROUTINE SMMV(N, A, IA, JA, b, c)
    implicit none
    INTEGER, intent(in) :: N, IA(*), JA(*)
    REAL*8, intent(in) :: A(*), b(*)
    REAL*8, intent(out) :: c(*)

    REAL*8 :: t
    INTEGER :: i, j

    DO i = 1, N
        t = 0.0d0
        DO j = IA(i), IA(i+1)-1
            t = t + A(j) * b(JA(j))
        END DO
        c(i)=t
    END DO
END SUBROUTINE SMMV

! Calcule c = A^T * b    (sparse matrix - CSR format)

SUBROUTINE SMMVT(N, A, IA, JA, b, c)
    implicit none
    INTEGER, intent(in) :: N, IA(*), JA(*)
    REAL*8, intent(in) :: A(*), b(*)
    REAL*8, intent(out) :: c(*)
    INTEGER :: i, j, k

    DO i = 1, N
        c(i) = 0.0D0
    ENDDO

    DO i = 1, N
        DO j = IA(i), IA(i+1)-1
            k = JA(j)
            c(k) = c(k) + A(j) * b(i)
        END DO
    END DO
END SUBROUTINE SMMVT

! Fournit H(i,j)   (où H est triu + diag)

REAL*8 FUNCTION HCALL(H, i, j)
    implicit none
    REAL*8, intent(in) :: H(*)
    INTEGER, intent(in) :: i,j

    HCALL = H( (j-1)*(j+2)/2 + i )
END FUNCTION HCALL


! Effectue H(i,j):=alpha  (où H est triu + diag)

SUBROUTINE HASSIGN(H, i, j, alpha)
    implicit none
    REAL*8, intent(out) :: H(*)
    REAL*8, intent(in)  :: alpha
    INTEGER, intent(in) :: i, j
    
    H( ((j-1)*(j+2))/2 + i ) = alpha
END SUBROUTINE HASSIGN
