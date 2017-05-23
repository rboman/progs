C+++++++++++++++++++++++++++++++++++++++
C
C  ALGO    Bubble sort
C  -------------------
C                  source : de Marneffe
C+++++++++++++++++++++++++++++++++++++++
C
      SUBROUTINE bblsort(NN,B)
C
      INTEGER NN,B,n,i,k,temp
      DIMENSION B(NN)
C
      n=NN
C
 10   IF(n.EQ.2) GOTO 99
         i=1
         k=1
 20      IF(i.EQ.n) GOTO 88
            IF(B(i).GT.B(i+1))THEN
               temp=B(i)
               B(i)=B(i+1)
               B(i+1)=temp
               k=i
            ENDIF
            i=i+1
         GOTO 20
 88      n=k+1  
      GOTO 10
C
 99   RETURN
      END