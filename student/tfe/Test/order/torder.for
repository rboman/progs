      program toder
    
      implicit real*8(a-h,o-z)

      integer NN, NNELEM, IA(200), JA(5000), ierr
      real*8  A(5000), b(200)
      integer nfirst,iperm(200),mask(200),riord(200),levels(200),
     *     nlev,maskval 

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


      nfirst=1
      iperm(1)=0
      riord(1)=98
      do i=1,nn
         mask(i)=1
      enddo
      maskval=1


      call BFS(nn,ja,ia,nfirst,iperm,mask,maskval,riord,levels,
     *     nlev)
      open(UNIT = 1, FILE = 'perm.m', STATUS = 'unknown')
      do i=1,nn
         WRITE(1,*)'P(',i,')=',riord(i),';'
      enddo
      close(UNIT=1)
      end