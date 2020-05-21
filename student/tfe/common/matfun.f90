!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
! Sous-routines & fonctions matricielles élémentaires
! 17.11.96
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

! Calcule c = A * b    (full matrix)

SUBROUTINE MatMult(N, A, b, c)
    implicit none
    INTEGER :: N, i, j
    REAL*8 :: A(N,N), b(N), c(N)

    DO i=1,N
        c(i)=0.0D0
        DO j=1,N
            c(i) = c(i) + A(i,j) * b(j)
        END DO
    END DO
    RETURN
END SUBROUTINE MatMult

! Calcule ProdScal = v2 * v2

REAL*8 function ProdScal(N, v1, v2)
    implicit none
    INTEGER :: N, i
    REAL*8 :: v1(*), v2(*), pro

    pro=0.0D0
    DO i=1,N
        pro=pro+v1(i)*v2(i)
    END DO
    ProdScal = pro
    RETURN
END function ProdScal

! Calcule VectNorm = norm2(v1)

REAL*8 function VectNorm(N, v1)
    implicit none
    INTEGER :: N
    REAL*8 :: v1(*), ProdScal

    VectNorm = DSQRT(ProdScal(N, v1, v1))
    RETURN
END function VectNorm

! Calcule v3 = v1 + v2

SUBROUTINE VectAdd(N, v1, v2, v3)
    implicit none
    INTEGER :: N, i
    REAL*8 :: v1(N), v2(N), v3(N)

    DO i=1,N
        v3(i)=v1(i)+v2(i)
    END DO
    RETURN
END SUBROUTINE VectAdd

! Calcule v3 = v1 - v2

SUBROUTINE VectSub(N, v1, v2, v3)
    implicit none
    INTEGER :: N, i
    REAL*8 :: v1(N), v2(N), v3(N)

    DO i=1,N
        v3(i)=v1(i)-v2(i)
    END DO
    RETURN
END SUBROUTINE VectSub

! Calcule v2 = alpha * v1    (alpha réel)

SUBROUTINE VectMul(N, v1, alpha, v2)
    implicit none
    INTEGER :: N, i
    REAL*8 :: v1(N), v2(N), alpha

    DO i=1,N
        v2(i)=v1(i)*alpha
    END DO
    RETURN
END SUBROUTINE VectMul

! Copie le vecteur v2 dans v1  (v1:=v2)

SUBROUTINE VectAssign(N, v1, v2)
    implicit none
    INTEGER :: N, i
    REAL*8 :: v1(*), v2(*)

    DO i=1,N
        v1(i)=v2(i)
    END DO
    RETURN
END SUBROUTINE VectAssign

! Affiche le vecteur v1 et attend un <ENTER>

SUBROUTINE VectAff(N,v1)
    implicit none
    INTEGER :: N, i
    REAL*8 :: v1(*)

    DO i = 1, N
        WRITE(*,*) i,':',v1(i)
    END DO
    write(*,*)'<ENTER>'
    read(*,*)
    RETURN
END SUBROUTINE VectAff

! Affiche le vecteur v1 et v2 
! et leur différence et attend un <ENTER>

SUBROUTINE VectAff2(N, v1, v2)
    implicit none
    INTEGER :: N, i
    REAL*8 :: v1(*), v2(*)

    DO i = 1, N
        WRITE(*,*) i,':',v1(i),v2(i), v2(i)-v1(i)
    END DO
    write(*,*)'<ENTER>'
    read(*,*)
    RETURN
END SUBROUTINE VectAff2


! Initialise la matrice A(N,M) à 0.0d0   (full matrix)

SUBROUTINE MatInit(N, M, A)
    implicit none
    INTEGER :: N, M, i, j 
    REAL*8 :: A(N,M)

    DO i = 1, N
        DO j = 1, M
            A(i,j) = 0.0D0
        END DO
    END DO
    RETURN 
END SUBROUTINE MatInit

! Initialise le vecteur v à 0.0d0

SUBROUTINE VectInit(N, v)
    implicit none
    INTEGER :: N, i 
    REAL*8 :: v(N)

    DO i = 1, N          
        v(i) = 0.0D0
    END DO
    RETURN
END SUBROUTINE VectInit

! Effectue y=a*x+y

SUBROUTINE DAXPY(N, A, X, Y)
    implicit none
    INTEGER :: I, N
    REAL*8  :: A,X(N),Y(N)

    IF (A .EQ. 0.0) RETURN
    DO I = 1, N
        Y(I) = A*X(I) + Y(I)
    ENDDO
    RETURN
END SUBROUTINE DAXPY
