C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C        Chargement du système à partir d'un fichier 
C              (SYSTEM2.BIN) créé par matlab   
C------------------------------------------------------------
C  return : ierr=0 si chargement ok.                 12.12.96
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      SUBROUTINE loadmat(A,IA,JA,b,ierr)

      integer NN, NNELEM, IA(*), JA(*), ierr
      real*8  A(*), b(*)


      open(UNIT = 1, FILE = 'system2.bin', STATUS = 'unknown')
      write(*,*)'Chargement du systeme : SYSTEM2.BIN'

C     Dimensions du système :

      read(1,*) NN
      read(1,*) NNELEM
      write(*,*)'Dimension :',NN
      write(*,*)'NNZ       :',NNELEM

C      if((NN.NE.NELEM)) then
C         write(*,*)'Out of memory !'
C         write(*,*)'Dim. max =',N
C         write(*,*)'NNZ  max =',NELEM
C         ierr=1
C      else

C        Chargement de la matrice :

         do 11 j = 1,NNELEM
            read(1,*) A(j)
            read(1,*) JA(j)
11       continue
         do 12 j = 1,NN+1
            read(1,*) IA(j)
12       continue 

C        Chargement du vecteur b :
   
         do 30 j = 1,NN
            read(1,*) b(j)
30       continue
   
C        Fermeture du fichier

         close (UNIT = 1)
         write(*,*)'Ok..'
         ierr=0
C      endif

      return
      end