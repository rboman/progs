!C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!C        Chargement du système à partir d'un fichier 
!C              (SYSTEM2.BIN) créé par matlab   
!C------------------------------------------------------------
!C  return : ierr=0 si chargement ok.                 12.12.96
!C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

subroutine loadmat(A, IA, JA, b, xref, ierr)

    implicit none

    integer NN, NNELEM, IA(*), JA(*), ierr
    double precision  A(*), b(*), xref(*)
    integer :: ios, j

    open(UNIT = 1, FILE = 'system2.bin',                    &
         ACTION='read', STATUS = 'old', IOSTAT=ios)
    write(*,*) 'ios=', ios
    if (ios .ne. 0) then
        write(*,*) "file 'system2.bin' cannot be opened!"
        stop 1
    end if

    write(*,*)'Chargement du systeme : SYSTEM2.BIN'

    ! Dimensions du systטme :

    read(1,*) NN
    read(1,*) NNELEM
    write(*,*)'Dimension :',NN
    write(*,*)'NNZ       :',NNELEM

    !      if((NN.NE.NELEM)) then
    !         write(*,*)'Out of memory !'
    !         write(*,*)'Dim. max =',N
    !         write(*,*)'NNZ  max =',NELEM
    !         ierr=1
    !      else

    ! Chargement de la matrice :

    do j = 1,NNELEM
        read(1,*) A(j)
        read(1,*) JA(j)
    end do
    do j = 1,NN+1
        read(1,*) IA(j)
    end do 

    ! Chargement du vecteur b :
   
    do j = 1,NN
        read(1,*) b(j)
    end do

    ! Chargement du vecteur xref :
   
    do j = 1,NN
        read(1,*) xref(j)
    end do

    ! Fermeture du fichier

    close (UNIT = 1)
    write(*,*)'Ok..'
    ierr=0

end subroutine loadmat
