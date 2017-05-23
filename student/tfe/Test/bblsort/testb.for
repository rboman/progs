      PROGRAM TESTB
C+++++++++++++++++++++++++++++++++++++++++
C
C Teste la routine bblsort
C
C+++++++++++++++++++++++++++++++++++++++++
C
      IMPLICIT REAL*8(A-H,O-Z)
C
      DIMENSION JJ(10)
C
      JJ(1)=5
      JJ(2)=90
      JJ(3)=18
      JJ(4)=2
      JJ(5)=40
      JJ(6)=800
      JJ(7)=8
      JJ(8)=5
      JJ(9)=16
      JJ(10)=1
      WRITE(*,*)'Avant bubble sort :'
      WRITE(*,*)JJ
      WRITE(*,*)'Apres bubble sort :'
      CALL bblsort(10,JJ)
      WRITE(*,*)JJ
C
      END


