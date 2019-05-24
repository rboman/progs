!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!   SOLVER : BICGSTAB (d'après TEMPLATES)           11.02.97
!
!   Utilise ILU0 & ILUTP
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

PROGRAM TEST_BICGSTAB

    IMPLICIT REAL*8(A-H,O-Z)

    parameter(N = 100, NELEM = 5000, iwk= 10000)

    double precision ::  A(NELEM)

    integer :: IA(N+1), JA(NELEM)

    double precision :: ALU(iwk)
    integer :: JLU(iwk), JU(N)

    double precision :: B(N),X(N)
    integer :: iw(2*N)

    double precision :: r(N),r2(N),p(N),p2(N),s(N),s2(N),t(N),v(N)
    integer :: iperm(2*N)
    double precision :: xref(N)

    integer :: ierr

    ierr = 0

    ! Chargement de la matrice A et du vecteur b ---------
    CALL loadmat(A, IA, JA, b, xref, ierr)

    ! Paramètres du solver -------------------------------
    epsilon = 1.0D-8
    it_max  = 800
    iflag   = 2
    ierr    = 0

    ! Paramètres du préconditionneur ---------------------
    droptol = 0.0
    lfil    = 40
    permtol = 0.1
    mbloc   = N 

    IF(iflag.EQ.1) THEN
        CALL ilu0(n, a, ja, ia, alu, jlu, ju, iw, ierr)
    ENDIF
    IF(iflag.EQ.2) THEN
        CALL ilutp(n, a, ja, ia, lfil, droptol, permtol, mbloc, alu,   &
                   jlu, ju, iwk, r, iw, iperm, ierr)
    ENDIF
    WRITE(*,*)ierr

    ! Appel de la sous-routine BICGSTAB -----------------

    CALL BICGSTAB(N, x, b, A, IA, JA, ALU, JLU, JU,    &
                  epsilon, it_max, r, r2, p, p2, s, s2, t, v,   &
                  IFLAG, ierr)

    ! Permutation de la solution (si pivotage employé) --

    IF(IFLAG.EQ.2) THEN
        DO i=1,N
            r(i)=x(iperm(n+i))
        END DO
        DO i=1,N
            x(i)=r(i)
        END DO
    ENDIF

    ! Sauvegarde du résultat vers MATLAB ----------------

    OPEN (UNIT = 1, FILE = 's_gmres.m', STATUS = 'UNKNOWN')
    DO i = 1, N
        WRITE(1,*) 'x(', i, ')=', x(i), ';'
    END DO
    CLOSE (UNIT = 1)

    ! Visualisation de la solution ----------------------

    WRITE(*,*)
    WRITE(*,*)' <ENTER> pour voir la solution'
    READ(*,*)

    CALL VectAff2(N, x, xref)

END PROGRAM TEST_BICGSTAB
